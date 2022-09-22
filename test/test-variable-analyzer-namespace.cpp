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

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;

namespace quick_lint_js {
namespace {
TEST(test_variable_analyzer_namespace, empty_namespace) {
  const char8 namespace_declaration[] = u8"NS";

  // namespace NS { }
  diag_collector v;
  variable_analyzer l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(namespace_declaration),
                               variable_kind::_namespace,
                               variable_init_kind::normal);
  l.visit_enter_namespace_scope();
  l.visit_exit_namespace_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer_namespace,
     namespace_name_is_visible_outside_namespace) {
  const char8 namespace_declaration[] = u8"NS";
  const char8 namespace_use[] = u8"NS";

  // namespace NS { }
  // NS;
  diag_collector v;
  variable_analyzer l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(namespace_declaration),
                               variable_kind::_namespace,
                               variable_init_kind::normal);
  l.visit_enter_namespace_scope();
  l.visit_exit_namespace_scope();
  l.visit_variable_use(identifier_of(namespace_use));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

// TODO(strager): Is this correct? TypeScript's compiler (as of v4.8.2)
// complains about accessing members of the namespace, but doesn't complain
// about referencing the namespace itself.
TEST(test_variable_analyzer_namespace,
     namespace_name_is_usable_before_namespace) {
  const char8 namespace_declaration[] = u8"NS";
  const char8 namespace_use[] = u8"NS";

  // NS;
  // namespace NS { }
  diag_collector v;
  variable_analyzer l(&v, &default_globals);
  l.visit_variable_use(identifier_of(namespace_use));
  l.visit_variable_declaration(identifier_of(namespace_declaration),
                               variable_kind::_namespace,
                               variable_init_kind::normal);
  l.visit_enter_namespace_scope();
  l.visit_exit_namespace_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer_namespace,
     namespace_name_is_visible_inside_namespace) {
  const char8 namespace_declaration[] = u8"NS";
  const char8 namespace_use[] = u8"NS";

  // namespace NS {
  //   NS;
  // }
  diag_collector v;
  variable_analyzer l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(namespace_declaration),
                               variable_kind::_namespace,
                               variable_init_kind::normal);
  l.visit_enter_namespace_scope();
  l.visit_variable_use(identifier_of(namespace_use));
  l.visit_exit_namespace_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer_namespace,
     variables_declared_inside_namespace_are_not_accessible_outside) {
  const char8 namespace_declaration[] = u8"NS";
  const char8 namespace_member_declaration[] = u8"C";
  const char8 namespace_member_use[] = u8"C";

  for (variable_kind var_kind : {variable_kind::_class, variable_kind::_var}) {
    SCOPED_TRACE(var_kind);
    // namespace NS {
    //   export class C {}
    // }
    // C;  // ERROR
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(namespace_declaration),
                                 variable_kind::_namespace,
                                 variable_init_kind::normal);
    l.visit_enter_namespace_scope();
    l.visit_variable_declaration(identifier_of(namespace_member_declaration),
                                 var_kind, variable_init_kind::normal);
    l.visit_exit_namespace_scope();
    l.visit_variable_use(identifier_of(namespace_member_use));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_SPAN(
                              diag_use_of_undeclared_variable, name,
                              span_of(namespace_member_use))));
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
