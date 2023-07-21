// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/variable-analyzer.h>
#include <quick-lint-js/identifier-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/variable-analyzer-support.h>

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
TEST(Test_Variable_Analyzer_Enum,
     member_initializers_can_reference_other_members) {
  const Char8 enum_declaration[] = u8"E";
  const Char8 member_use[] = u8"A";

  // enum E {
  //   A = 42,
  //   B = A,
  // }
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(enum_declaration),
                               Variable_Kind::_enum,
                               Variable_Declaration_Flags::none);
  l.visit_enter_enum_scope();
  l.visit_variable_use(identifier_of(member_use));
  l.visit_exit_enum_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer_Enum, enum_can_merge_with_another_enum) {
  const Char8 enum_declaration_1[] = u8"E";
  const Char8 enum_declaration_2[] = u8"E";

  // enum E {}
  // enum E {}
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(enum_declaration_1),
                               Variable_Kind::_enum,
                               Variable_Declaration_Flags::none);
  l.visit_enter_enum_scope();
  l.visit_exit_enum_scope();
  l.visit_variable_declaration(identifier_of(enum_declaration_2),
                               Variable_Kind::_enum,
                               Variable_Declaration_Flags::none);
  l.visit_enter_enum_scope();
  l.visit_exit_enum_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer_Enum, enum_can_shadow_catch_variables) {
  const Char8 catch_declaration[] = u8"e";
  const Char8 enum_declaration[] = u8"e";

  // try {
  // } catch (e) {
  //   enum e {}
  // }
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_block_scope();
  l.visit_exit_block_scope();
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(catch_declaration),
                               Variable_Kind::_catch,
                               Variable_Declaration_Flags::none);
  l.visit_variable_declaration(identifier_of(enum_declaration),
                               Variable_Kind::_enum,
                               Variable_Declaration_Flags::none);
  l.visit_enter_enum_scope();
  l.visit_exit_enum_scope();
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer_Enum,
     enum_conflicts_with_most_variables_in_same_scope) {
  const Char8 enum_declaration[] = u8"E";
  const Char8 other_declaration[] = u8"E";

  for (Variable_Kind other_kind : {
           Variable_Kind::_arrow_parameter,
           Variable_Kind::_class,
           Variable_Kind::_const,
           Variable_Kind::_function,
           Variable_Kind::_function_parameter,
           Variable_Kind::_generic_parameter,
           Variable_Kind::_import,
           // FIXME(strager): Is _index_signature_parameter even possible?
           Variable_Kind::_index_signature_parameter,
           Variable_Kind::_interface,
           Variable_Kind::_let,
           Variable_Kind::_var,
       }) {
    SCOPED_TRACE(other_kind);

    {
      // var E;
      // enum E {}  // ERROR
      Diag_Collector v;
      Variable_Analyzer l(&v, &default_globals, javascript_var_options);
      l.visit_variable_declaration(identifier_of(other_declaration), other_kind,
                                   Variable_Declaration_Flags::none);
      l.visit_variable_declaration(identifier_of(enum_declaration),
                                   Variable_Kind::_enum,
                                   Variable_Declaration_Flags::none);
      l.visit_enter_enum_scope();
      l.visit_exit_enum_scope();
      l.visit_end_of_module();

      EXPECT_THAT(v.errors,
                  ElementsAreArray({
                      DIAG_TYPE_2_SPANS(
                          Diag_Redeclaration_Of_Variable,            //
                          redeclaration, span_of(enum_declaration),  //
                          original_declaration, span_of(other_declaration)),
                  }));
    }

    {
      // enum E {}
      // var E;     // ERROR
      Diag_Collector v;
      Variable_Analyzer l(&v, &default_globals, javascript_var_options);
      l.visit_variable_declaration(identifier_of(enum_declaration),
                                   Variable_Kind::_enum,
                                   Variable_Declaration_Flags::none);
      l.visit_enter_enum_scope();
      l.visit_exit_enum_scope();
      l.visit_variable_declaration(identifier_of(other_declaration), other_kind,
                                   Variable_Declaration_Flags::none);
      l.visit_end_of_module();

      EXPECT_THAT(v.errors,
                  ElementsAreArray({
                      DIAG_TYPE_2_SPANS(
                          Diag_Redeclaration_Of_Variable,             //
                          redeclaration, span_of(other_declaration),  //
                          original_declaration, span_of(enum_declaration)),
                  }));
    }
  }
}

TEST(Test_Variable_Analyzer_Enum, function_shadows_enum_in_outer_scope) {
  const Char8 enum_declaration[] = u8"E";
  const Char8 function_declaration[] = u8"E";

  // enum E {}
  // {
  //   function E() {}
  // }
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(enum_declaration),
                               Variable_Kind::_enum,
                               Variable_Declaration_Flags::none);
  l.visit_enter_enum_scope();
  l.visit_exit_enum_scope();
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(function_declaration),
                               Variable_Kind::_function,
                               Variable_Declaration_Flags::none);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer_Enum, var_conflicts_with_enum_in_outer_scope) {
  const Char8 enum_declaration[] = u8"e";
  const Char8 var_declaration[] = u8"e";

  {
    // enum e {}
    // {
    //   var e;   // ERROR
    // }
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(enum_declaration),
                                 Variable_Kind::_enum,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_enum_scope();
    l.visit_exit_enum_scope();
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 Variable_Kind::_var,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_2_SPANS(Diag_Redeclaration_Of_Variable,           //
                              redeclaration, span_of(var_declaration),  //
                              original_declaration, span_of(enum_declaration)),
        }));
  }

  {
    // {
    //   var e;
    // }
    // enum e {}  // ERROR
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 Variable_Kind::_var,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_block_scope();
    l.visit_variable_declaration(identifier_of(enum_declaration),
                                 Variable_Kind::_enum,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_enum_scope();
    l.visit_exit_enum_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_2_SPANS(Diag_Redeclaration_Of_Variable,            //
                              redeclaration, span_of(enum_declaration),  //
                              original_declaration, span_of(var_declaration)),
        }));
  }
}

TEST(Test_Variable_Analyzer_Enum, enum_shadows_most_variables_in_outer_scope) {
  const Char8 outer_declaration[] = u8"E";
  const Char8 enum_declaration[] = u8"E";

  for (Variable_Kind outer_kind : {
           Variable_Kind::_arrow_parameter,
           Variable_Kind::_class,
           Variable_Kind::_const,
           Variable_Kind::_function,
           Variable_Kind::_function_parameter,
           Variable_Kind::_generic_parameter,
           Variable_Kind::_import,
           // FIXME(strager): Is _index_signature_parameter even possible?
           Variable_Kind::_index_signature_parameter,
           Variable_Kind::_interface,
           Variable_Kind::_let,
           Variable_Kind::_var,
       }) {
    SCOPED_TRACE(outer_kind);
    // var E;
    // {
    //   enum E {}
    // }
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(outer_declaration), outer_kind,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(enum_declaration),
                                 Variable_Kind::_enum,
                                 Variable_Declaration_Flags::none);
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
