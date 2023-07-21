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

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
TEST(Test_Variable_Analyzer_Typeof,
     using_undeclared_variable_in_typeof_is_not_an_error) {
  const Char8 use[] = u8"v";

  // typeof v;
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_typeof_use(identifier_of(use));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer_Typeof, typeof_declares_variable_automagically) {
  const Char8 typeof_use[] = u8"v";
  const Char8 other_use[] = u8"v";

  // typeof v;
  // v;
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_typeof_use(identifier_of(typeof_use));
  l.visit_variable_use(identifier_of(other_use));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer_Typeof,
     typeof_declares_variable_automagically_in_parent_function) {
  const Char8 use_before[] = u8"v";
  const Char8 typeof_use[] = u8"v";
  const Char8 use_after[] = u8"v";

  // v;
  // (() => {
  //   typeof v;
  // });
  // v;
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_use(identifier_of(use_before));
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_typeof_use(identifier_of(typeof_use));
  l.visit_exit_function_scope();
  l.visit_variable_use(identifier_of(use_after));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer_Typeof,
     typeof_refers_to_already_declared_variable) {
  const Char8 declaration[] = u8"v";
  const Char8 use[] = u8"v";

  // let v;
  // typeof v;
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(declaration), Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_variable_typeof_use(identifier_of(use));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer_Typeof,
     typeof_variable_declared_later_is_an_error) {
  const Char8 declaration[] = u8"v";
  const Char8 use[] = u8"v";

  // typeof v;  // ERROR
  // let v;
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_typeof_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(declaration), Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_2_SPANS(Diag_Variable_Used_Before_Declaration,  //
                                    use, span_of(use),                      //
                                    declaration, span_of(declaration)),
              }));
}

TEST(
    Test_Variable_Analyzer_Typeof,
    typeof_already_declared_variable_does_not_declare_variable_in_parent_function) {
  const Char8 use_before[] = u8"v";
  const Char8 declaration[] = u8"v";
  const Char8 typeof_use[] = u8"v";
  const Char8 use_after[] = u8"v";

  // v;           // ERROR
  // (() => {
  //   let v;
  //   typeof v;
  // });
  // v;           // ERROR
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_use(identifier_of(use_before));
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_declaration(identifier_of(declaration), Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_variable_typeof_use(identifier_of(typeof_use));
  l.visit_exit_function_scope();
  l.visit_variable_use(identifier_of(use_after));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAreArray({
                            DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Variable,
                                           name, span_of(use_before)),
                            DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Variable,
                                           name, span_of(use_after)),
                        }));
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
