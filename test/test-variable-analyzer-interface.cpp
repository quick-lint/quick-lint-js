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
TEST(test_variable_analyzer_interface,
     interface_body_can_reference_types_outside) {
  const char8 interface_declaration[] = u8"I";
  const char8 type_declaration[] = u8"C";
  const char8 type_use[] = u8"C";
  const char8 method_name[] = u8"method";

  {
    // import {C} from "other-module";
    // interface I {
    //   method(): C;
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(type_declaration),
                                 variable_kind::_import,
                                 variable_init_kind::normal);
    l.visit_variable_declaration(identifier_of(interface_declaration),
                                 variable_kind::_interface,
                                 variable_init_kind::normal);
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
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(interface_declaration),
                                 variable_kind::_interface,
                                 variable_init_kind::normal);
    l.visit_enter_interface_scope();
    l.visit_property_declaration(identifier_of(method_name));
    l.visit_enter_function_scope();
    l.visit_variable_type_use(identifier_of(type_use));
    l.visit_exit_function_scope();
    l.visit_exit_interface_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_SPAN(diag_use_of_undeclared_type, name,
                                           span_of(type_use))));
  }
}

TEST(test_variable_analyzer_interface,
     generic_interface_parameters_are_usable_inside) {
  const char8 interface_declaration[] = u8"I";
  const char8 parameter_declaration[] = u8"T";
  const char8 parameter_use[] = u8"T";
  const char8 method_name[] = u8"method";

  {
    // interface I<T> {
    //   method(): T;
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(interface_declaration),
                                 variable_kind::_interface,
                                 variable_init_kind::normal);
    l.visit_enter_interface_scope();
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_generic_parameter,
                                 variable_init_kind::normal);
    l.visit_property_declaration(identifier_of(method_name));
    l.visit_enter_function_scope();
    l.visit_variable_type_use(identifier_of(parameter_use));
    l.visit_exit_function_scope();
    l.visit_exit_interface_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_interface,
     interface_index_signature_can_use_outside_types) {
  const char8 type_declaration[] = u8"C";
  const char8 interface_declaration[] = u8"I";
  const char8 index_declaration[] = u8"index";
  const char8 type_use_1[] = u8"C";
  const char8 type_use_2[] = u8"C";

  {
    // import {C} from "other-module";
    // interface I {
    //   [index: C]: C;
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(type_declaration),
                                 variable_kind::_import,
                                 variable_init_kind::normal);
    l.visit_variable_declaration(identifier_of(interface_declaration),
                                 variable_kind::_interface,
                                 variable_init_kind::normal);
    l.visit_enter_interface_scope();
    l.visit_enter_index_signature_scope();
    l.visit_variable_type_use(identifier_of(type_use_1));
    l.visit_variable_declaration(identifier_of(index_declaration),
                                 variable_kind::_index_signature_parameter,
                                 variable_init_kind::normal);
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
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(interface_declaration),
                                 variable_kind::_interface,
                                 variable_init_kind::normal);
    l.visit_enter_interface_scope();
    l.visit_enter_index_signature_scope();
    l.visit_variable_type_use(identifier_of(type_use_1));
    l.visit_variable_declaration(identifier_of(index_declaration),
                                 variable_kind::_index_signature_parameter,
                                 variable_init_kind::normal);
    l.visit_variable_type_use(identifier_of(type_use_2));
    l.visit_exit_index_signature_scope();
    l.visit_exit_interface_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, UnorderedElementsAre(
                              DIAG_TYPE_SPAN(diag_use_of_undeclared_type, name,
                                             span_of(type_use_1)),
                              DIAG_TYPE_SPAN(diag_use_of_undeclared_type, name,
                                             span_of(type_use_2))));
  }
}

TEST(test_variable_analyzer_interface,
     interface_index_signature_variable_is_usable_inside) {
  const char8 interface_declaration[] = u8"I";
  const char8 index_declaration[] = u8"index";
  const char8 index_use[] = u8"index";

  {
    // interface I {
    //   [index: number]: typeof index;
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(interface_declaration),
                                 variable_kind::_interface,
                                 variable_init_kind::normal);
    l.visit_enter_interface_scope();
    l.visit_enter_index_signature_scope();
    l.visit_variable_declaration(identifier_of(index_declaration),
                                 variable_kind::_index_signature_parameter,
                                 variable_init_kind::normal);
    l.visit_variable_use(identifier_of(index_use));
    l.visit_exit_index_signature_scope();
    l.visit_exit_interface_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_interface,
     interface_index_signature_variable_is_not_usable_outside) {
  const char8 interface_declaration[] = u8"I";
  const char8 index_declaration[] = u8"index";
  const char8 property_declaration[] = u8"index";
  const char8 index_use_inside_interface[] = u8"index";
  const char8 index_use_outside_interface[] = u8"index";

  {
    // interface I {
    //   [index: number]: number;
    //   other: typeof index;  // ERROR
    // }
    // index;  // ERROR
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(interface_declaration),
                                 variable_kind::_interface,
                                 variable_init_kind::normal);
    l.visit_enter_interface_scope();
    l.visit_enter_index_signature_scope();
    l.visit_variable_declaration(identifier_of(index_declaration),
                                 variable_kind::_index_signature_parameter,
                                 variable_init_kind::normal);
    l.visit_exit_index_signature_scope();
    l.visit_property_declaration(identifier_of(property_declaration));
    l.visit_variable_use(identifier_of(index_use_inside_interface));
    l.visit_exit_interface_scope();
    l.visit_variable_use(identifier_of(index_use_outside_interface));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                UnorderedElementsAre(
                    DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name,
                                   span_of(index_use_inside_interface)),
                    DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name,
                                   span_of(index_use_outside_interface))));
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
