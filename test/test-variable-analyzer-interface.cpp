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
using ::testing::UnorderedElementsAreArray;

namespace quick_lint_js {
namespace {
TEST(Test_Variable_Analyzer_Interface,
     interface_body_can_reference_types_outside) {
  const Char8 interface_declaration[] = u8"I";
  const Char8 type_declaration[] = u8"C";
  const Char8 type_use[] = u8"C";
  const Char8 method_name[] = u8"method";

  {
    // import {C} from "other-module";
    // interface I {
    //   method(): C;
    // }
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(type_declaration),
                                 Variable_Kind::_import,
                                 Variable_Declaration_Flags::none);
    l.visit_variable_declaration(identifier_of(interface_declaration),
                                 Variable_Kind::_interface,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_interface_scope();
    l.visit_property_declaration(identifier_of(method_name));
    l.visit_enter_function_scope();
    l.visit_variable_type_use(identifier_of(type_use));
    l.visit_exit_function_scope();
    l.visit_exit_interface_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // interface I {
    //   method(): C;  // ERROR
    // }
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(interface_declaration),
                                 Variable_Kind::_interface,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_interface_scope();
    l.visit_property_declaration(identifier_of(method_name));
    l.visit_enter_function_scope();
    l.visit_variable_type_use(identifier_of(type_use));
    l.visit_exit_function_scope();
    l.visit_exit_interface_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAreArray({
                              DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Type, name,
                                             span_of(type_use)),
                          }));
  }
}

TEST(Test_Variable_Analyzer_Interface,
     generic_interface_parameters_are_usable_inside) {
  const Char8 interface_declaration[] = u8"I";
  const Char8 parameter_declaration[] = u8"T";
  const Char8 parameter_use[] = u8"T";
  const Char8 method_name[] = u8"method";

  {
    // interface I<T> {
    //   method(): T;
    // }
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(interface_declaration),
                                 Variable_Kind::_interface,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_interface_scope();
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 Variable_Kind::_generic_parameter,
                                 Variable_Declaration_Flags::none);
    l.visit_property_declaration(identifier_of(method_name));
    l.visit_enter_function_scope();
    l.visit_variable_type_use(identifier_of(parameter_use));
    l.visit_exit_function_scope();
    l.visit_exit_interface_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer_Interface,
     interface_index_signature_can_use_outside_types) {
  const Char8 type_declaration[] = u8"C";
  const Char8 interface_declaration[] = u8"I";
  const Char8 index_declaration[] = u8"index";
  const Char8 type_use_1[] = u8"C";
  const Char8 type_use_2[] = u8"C";

  {
    // import {C} from "other-module";
    // interface I {
    //   [index: C]: C;
    // }
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(type_declaration),
                                 Variable_Kind::_import,
                                 Variable_Declaration_Flags::none);
    l.visit_variable_declaration(identifier_of(interface_declaration),
                                 Variable_Kind::_interface,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_interface_scope();
    l.visit_enter_index_signature_scope();
    l.visit_variable_type_use(identifier_of(type_use_1));
    l.visit_variable_declaration(identifier_of(index_declaration),
                                 Variable_Kind::_index_signature_parameter,
                                 Variable_Declaration_Flags::none);
    l.visit_variable_type_use(identifier_of(type_use_2));
    l.visit_exit_index_signature_scope();
    l.visit_exit_interface_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // interface I {
    //   [index: C]: C;  // ERROR
    // }
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(interface_declaration),
                                 Variable_Kind::_interface,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_interface_scope();
    l.visit_enter_index_signature_scope();
    l.visit_variable_type_use(identifier_of(type_use_1));
    l.visit_variable_declaration(identifier_of(index_declaration),
                                 Variable_Kind::_index_signature_parameter,
                                 Variable_Declaration_Flags::none);
    l.visit_variable_type_use(identifier_of(type_use_2));
    l.visit_exit_index_signature_scope();
    l.visit_exit_interface_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, UnorderedElementsAreArray({
                              DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Type, name,
                                             span_of(type_use_1)),
                              DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Type, name,
                                             span_of(type_use_2)),
                          }));
  }
}

TEST(Test_Variable_Analyzer_Interface,
     interface_index_signature_variable_is_usable_inside) {
  const Char8 interface_declaration[] = u8"I";
  const Char8 index_declaration[] = u8"index";
  const Char8 index_use[] = u8"index";

  {
    // interface I {
    //   [index: number]: typeof index;
    // }
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(interface_declaration),
                                 Variable_Kind::_interface,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_interface_scope();
    l.visit_enter_index_signature_scope();
    l.visit_variable_declaration(identifier_of(index_declaration),
                                 Variable_Kind::_index_signature_parameter,
                                 Variable_Declaration_Flags::none);
    l.visit_variable_use(identifier_of(index_use));
    l.visit_exit_index_signature_scope();
    l.visit_exit_interface_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer_Interface,
     interface_index_signature_variable_is_not_usable_outside) {
  const Char8 interface_declaration[] = u8"I";
  const Char8 index_declaration[] = u8"index";
  const Char8 property_declaration[] = u8"index";
  const Char8 index_use_inside_interface[] = u8"index";
  const Char8 index_use_outside_interface[] = u8"index";

  {
    // interface I {
    //   [index: number]: number;
    //   other: typeof index;  // ERROR
    // }
    // index;  // ERROR
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(interface_declaration),
                                 Variable_Kind::_interface,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_interface_scope();
    l.visit_enter_index_signature_scope();
    l.visit_variable_declaration(identifier_of(index_declaration),
                                 Variable_Kind::_index_signature_parameter,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_index_signature_scope();
    l.visit_property_declaration(identifier_of(property_declaration));
    l.visit_variable_use(identifier_of(index_use_inside_interface));
    l.visit_exit_interface_scope();
    l.visit_variable_use(identifier_of(index_use_outside_interface));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                UnorderedElementsAreArray({
                    DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Variable, name,
                                   span_of(index_use_inside_interface)),
                    DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Variable, name,
                                   span_of(index_use_outside_interface)),
                }));
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
