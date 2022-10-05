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
TEST(test_variable_analyzer_arguments,
     arguments_magic_variable_is_usable_within_functions) {
  const char8 arguments_use[] = u8"arguments";

  // (function() {
  //   arguments;
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(arguments_use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer_arguments,
     arguments_magic_variable_is_unusable_in_global_scope) {
  const char8 arguments_use[] = u8"arguments";

  // arguments;
  diag_collector v;
  variable_analyzer l(&v, &default_globals);
  l.visit_variable_use(identifier_of(arguments_use));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAre(DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name,
                                         span_of(arguments_use))));
}

TEST(test_variable_analyzer_arguments,
     parameter_named_arguments_does_not_conflict) {
  const char8 parameter_declaration[] = u8"arguments";
  const char8 parameter_use[] = u8"arguments";

  // (function(arguments) {
  //   arguments;
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_variable_declaration(identifier_of(parameter_declaration),
                               variable_kind::_function_parameter,
                               variable_init_kind::normal);
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(parameter_use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer_arguments,
     parameter_default_values_can_reference_arguments) {
  const char8 parameter_declaration[] = u8"p";
  const char8 parameter_default_value[] = u8"arguments";

  {
    // (function(p = arguments) {
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_variable_use(identifier_of(parameter_default_value));
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_function_parameter,
                                 variable_init_kind::normal);
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  // 'arguments' refers to magic-arguments, not a local variable. If 'arguments'
  // referred to a local variable, this test would fail with a
  // use-before-declaration error.
  {
    const char8 local_declaration[] = u8"arguments";

    // (function(p = arguments) {
    //   let arguments;
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_variable_use(identifier_of(parameter_default_value));
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_function_parameter,
                                 variable_init_kind::normal);
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(local_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::normal);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_arguments,
     var_does_not_conflict_with_magic_arguments) {
  const char8 arguments_declaration[] = u8"arguments";

  // (function() {
  //   var arguments;
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_declaration(identifier_of(arguments_declaration),
                               variable_kind::_var, variable_init_kind::normal);
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer_arguments, let_shadows_magic_arguments) {
  for (variable_kind kind : {variable_kind::_const, variable_kind::_let}) {
    const char8 arguments_declaration[] = u8"arguments";

    // (function() {
    //   let arguments;
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(arguments_declaration), kind,
                                 variable_init_kind::normal);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  for (variable_kind kind : {variable_kind::_const, variable_kind::_let}) {
    const char8 arguments_declaration[] = u8"arguments";
    const char8 arguments_use[] = u8"arguments";

    // (function() {
    //   arguments;      // ERROR
    //   let arguments;
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(arguments_use));
    l.visit_variable_declaration(identifier_of(arguments_declaration), kind,
                                 variable_init_kind::normal);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_2_SPANS(
                    diag_variable_used_before_declaration,  //
                    use, span_of(arguments_use),            //
                    declaration, span_of(arguments_declaration))));
  }
}

TEST(test_variable_analyzer_arguments, function_shadows_magic_arguments) {
  const char8 arguments_declaration[] = u8"arguments";

  // (function() {
  //   function arguments() {}
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_declaration(identifier_of(arguments_declaration),
                               variable_kind::_function,
                               variable_init_kind::normal);
  l.visit_enter_function_scope();
  l.visit_exit_function_scope();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer_arguments, catch_variable_shadows_magic_arguments) {
  const char8 arguments_declaration[] = u8"arguments";

  // (function() {
  //   try {
  //   } catch (arguments) {
  //   }
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_enter_block_scope();
  l.visit_exit_block_scope();
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(arguments_declaration),
                               variable_kind::_catch,
                               variable_init_kind::normal);
  l.visit_exit_block_scope();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

// TODO(#204): 'arguments' should not be declared in arrow functions.
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
