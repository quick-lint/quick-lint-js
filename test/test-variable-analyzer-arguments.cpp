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
using ::testing::ElementsAreArray;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;

namespace quick_lint_js {
namespace {
TEST(Test_Variable_Analyzer_Arguments,
     arguments_magic_variable_is_usable_within_functions) {
  const Char8 arguments_use[] = u8"arguments";

  // (function() {
  //   arguments;
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(arguments_use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer_Arguments,
     arguments_magic_variable_is_unusable_in_global_scope) {
  const Char8 arguments_use[] = u8"arguments";

  // arguments;
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_use(identifier_of(arguments_use));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAreArray({
                            DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Variable,
                                           name, span_of(arguments_use)),
                        }));
}

TEST(Test_Variable_Analyzer_Arguments,
     parameter_named_arguments_does_not_conflict) {
  const Char8 parameter_declaration[] = u8"arguments";
  const Char8 parameter_use[] = u8"arguments";

  // (function(arguments) {
  //   arguments;
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_variable_declaration(identifier_of(parameter_declaration),
                               Variable_Kind::_function_parameter,
                               Variable_Declaration_Flags::none);
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(parameter_use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer_Arguments,
     parameter_default_values_can_reference_arguments) {
  const Char8 parameter_declaration[] = u8"p";
  const Char8 parameter_default_value[] = u8"arguments";

  {
    // (function(p = arguments) {
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_variable_use(identifier_of(parameter_default_value));
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 Variable_Kind::_function_parameter,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  // 'arguments' refers to magic-arguments, not a local variable. If 'arguments'
  // referred to a local variable, this test would fail with a
  // use-before-declaration error.
  {
    const Char8 local_declaration[] = u8"arguments";

    // (function(p = arguments) {
    //   let arguments;
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_variable_use(identifier_of(parameter_default_value));
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 Variable_Kind::_function_parameter,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(local_declaration),
                                 Variable_Kind::_let,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer_Arguments,
     var_does_not_conflict_with_magic_arguments) {
  const Char8 arguments_declaration[] = u8"arguments";

  // (function() {
  //   var arguments;
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_declaration(identifier_of(arguments_declaration),
                               Variable_Kind::_var,
                               Variable_Declaration_Flags::none);
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer_Arguments, let_shadows_magic_arguments) {
  for (Variable_Kind kind : {Variable_Kind::_const, Variable_Kind::_let}) {
    const Char8 arguments_declaration[] = u8"arguments";

    // (function() {
    //   let arguments;
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(arguments_declaration), kind,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  for (Variable_Kind kind : {Variable_Kind::_const, Variable_Kind::_let}) {
    const Char8 arguments_declaration[] = u8"arguments";
    const Char8 arguments_use[] = u8"arguments";

    // (function() {
    //   arguments;      // ERROR
    //   let arguments;
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(arguments_use));
    l.visit_variable_declaration(identifier_of(arguments_declaration), kind,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_2_SPANS(Diag_Variable_Used_Before_Declaration,  //
                              use, span_of(arguments_use),            //
                              declaration, span_of(arguments_declaration)),
        }));
  }
}

TEST(Test_Variable_Analyzer_Arguments, function_shadows_magic_arguments) {
  const Char8 arguments_declaration[] = u8"arguments";

  // (function() {
  //   function arguments() {}
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_declaration(identifier_of(arguments_declaration),
                               Variable_Kind::_function,
                               Variable_Declaration_Flags::none);
  l.visit_enter_function_scope();
  l.visit_exit_function_scope();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer_Arguments, catch_variable_shadows_magic_arguments) {
  const Char8 arguments_declaration[] = u8"arguments";

  // (function() {
  //   try {
  //   } catch (arguments) {
  //   }
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_enter_block_scope();
  l.visit_exit_block_scope();
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(arguments_declaration),
                               Variable_Kind::_catch,
                               Variable_Declaration_Flags::none);
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
