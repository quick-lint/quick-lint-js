// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/configuration/configuration.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/fe/lint.h>
#include <quick-lint-js/global-declared-variable-set.h>
#include <quick-lint-js/identifier-support.h>
#include <quick-lint-js/port/char8.h>

using ::testing::ElementsAre;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
global_declared_variable_set default_globals = configuration().globals();

TEST(test_lint_enum, member_initializers_can_reference_other_members) {
  const char8 enum_declaration[] = u8"E";
  const char8 member_use[] = u8"A";

  // enum E {
  //   A = 42,
  //   B = A,
  // }
  diag_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(enum_declaration),
                               variable_kind::_enum,
                               variable_init_kind::normal);
  l.visit_enter_enum_scope();
  l.visit_variable_use(identifier_of(member_use));
  l.visit_exit_enum_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint_enum, enum_can_merge_with_another_enum) {
  const char8 enum_declaration_1[] = u8"E";
  const char8 enum_declaration_2[] = u8"E";

  // enum E {}
  // enum E {}
  diag_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(enum_declaration_1),
                               variable_kind::_enum,
                               variable_init_kind::normal);
  l.visit_enter_enum_scope();
  l.visit_exit_enum_scope();
  l.visit_variable_declaration(identifier_of(enum_declaration_2),
                               variable_kind::_enum,
                               variable_init_kind::normal);
  l.visit_enter_enum_scope();
  l.visit_exit_enum_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint_enum, enum_can_shadow_catch_variables) {
  const char8 catch_declaration[] = u8"e";
  const char8 enum_declaration[] = u8"e";

  // try {
  // } catch (e) {
  //   enum e {}
  // }
  diag_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_block_scope();
  l.visit_exit_block_scope();
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(catch_declaration),
                               variable_kind::_catch,
                               variable_init_kind::normal);
  l.visit_variable_declaration(identifier_of(enum_declaration),
                               variable_kind::_enum,
                               variable_init_kind::normal);
  l.visit_enter_enum_scope();
  l.visit_exit_enum_scope();
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint_enum, enum_conflicts_with_most_variables_in_same_scope) {
  const char8 enum_declaration[] = u8"E";
  const char8 other_declaration[] = u8"E";

  for (variable_kind other_kind : {
           variable_kind::_class,
           variable_kind::_const,
           variable_kind::_function,
           variable_kind::_generic_parameter,
           variable_kind::_import,
           variable_kind::_interface,
           variable_kind::_let,
           variable_kind::_parameter,
           variable_kind::_var,
       }) {
    SCOPED_TRACE(other_kind);

    {
      // var E;
      // enum E {}  // ERROR
      diag_collector v;
      linter l(&v, &default_globals);
      l.visit_variable_declaration(identifier_of(other_declaration), other_kind,
                                   variable_init_kind::normal);
      l.visit_variable_declaration(identifier_of(enum_declaration),
                                   variable_kind::_enum,
                                   variable_init_kind::normal);
      l.visit_enter_enum_scope();
      l.visit_exit_enum_scope();
      l.visit_end_of_module();

      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_2_FIELDS(
                      diag_redeclaration_of_variable,                 //
                      redeclaration, span_matcher(enum_declaration),  //
                      original_declaration, span_matcher(other_declaration))));
    }

    {
      // enum E {}
      // var E;     // ERROR
      diag_collector v;
      linter l(&v, &default_globals);
      l.visit_variable_declaration(identifier_of(enum_declaration),
                                   variable_kind::_enum,
                                   variable_init_kind::normal);
      l.visit_enter_enum_scope();
      l.visit_exit_enum_scope();
      l.visit_variable_declaration(identifier_of(other_declaration), other_kind,
                                   variable_init_kind::normal);
      l.visit_end_of_module();

      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_2_FIELDS(
                      diag_redeclaration_of_variable,                  //
                      redeclaration, span_matcher(other_declaration),  //
                      original_declaration, span_matcher(enum_declaration))));
    }
  }
}

TEST(test_lint_enum, function_shadows_enum_in_outer_scope) {
  const char8 enum_declaration[] = u8"E";
  const char8 function_declaration[] = u8"E";

  // enum E {}
  // {
  //   function E() {}
  // }
  diag_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(enum_declaration),
                               variable_kind::_enum,
                               variable_init_kind::normal);
  l.visit_enter_enum_scope();
  l.visit_exit_enum_scope();
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(function_declaration),
                               variable_kind::_function,
                               variable_init_kind::normal);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint_enum, var_conflicts_with_enum_in_outer_scope) {
  const char8 enum_declaration[] = u8"e";
  const char8 var_declaration[] = u8"e";

  {
    // enum e {}
    // {
    //   var e;   // ERROR
    // }
    diag_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(enum_declaration),
                                 variable_kind::_enum,
                                 variable_init_kind::normal);
    l.visit_enter_enum_scope();
    l.visit_exit_enum_scope();
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 variable_kind::_var,
                                 variable_init_kind::normal);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_2_FIELDS(
                    diag_redeclaration_of_variable,                //
                    redeclaration, span_matcher(var_declaration),  //
                    original_declaration, span_matcher(enum_declaration))));
  }

  {
    // {
    //   var e;
    // }
    // enum e {}  // ERROR
    diag_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 variable_kind::_var,
                                 variable_init_kind::normal);
    l.visit_exit_block_scope();
    l.visit_variable_declaration(identifier_of(enum_declaration),
                                 variable_kind::_enum,
                                 variable_init_kind::normal);
    l.visit_enter_enum_scope();
    l.visit_exit_enum_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_2_FIELDS(
                    diag_redeclaration_of_variable,                 //
                    redeclaration, span_matcher(enum_declaration),  //
                    original_declaration, span_matcher(var_declaration))));
  }
}

TEST(test_lint_enum, enum_shadows_most_variables_in_outer_scope) {
  const char8 outer_declaration[] = u8"E";
  const char8 enum_declaration[] = u8"E";

  for (variable_kind outer_kind : {
           variable_kind::_class,
           variable_kind::_const,
           variable_kind::_function,
           variable_kind::_generic_parameter,
           variable_kind::_import,
           variable_kind::_interface,
           variable_kind::_let,
           variable_kind::_parameter,
           variable_kind::_var,
       }) {
    SCOPED_TRACE(outer_kind);
    // var E;
    // {
    //   enum E {}
    // }
    diag_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(outer_declaration), outer_kind,
                                 variable_init_kind::normal);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(enum_declaration),
                                 variable_kind::_enum,
                                 variable_init_kind::normal);
    l.visit_enter_enum_scope();
    l.visit_exit_enum_scope();
    l.visit_exit_block_scope();
    l.visit_end_of_module();

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
