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
using ::testing::ElementsAreArray;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;

// This file contains tests for multiple declarations with the same name.
namespace quick_lint_js {
namespace {
TEST(test_variable_analyzer_multiple_declarations,
     enum_and_namespace_do_not_conflict) {
  const char8 namespace_declaration[] = u8"A";
  const char8 enum_declaration[] = u8"A";

  {
    // namespace A {}
    // enum A {}
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(namespace_declaration),
                                 variable_kind::_namespace,
                                 variable_init_kind::normal);
    l.visit_enter_namespace_scope();
    l.visit_exit_namespace_scope();
    l.visit_variable_declaration(identifier_of(enum_declaration),
                                 variable_kind::_enum,
                                 variable_init_kind::normal);
    l.visit_enter_enum_scope();
    l.visit_exit_enum_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // enum A {}
    // namespace A {}
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(enum_declaration),
                                 variable_kind::_enum,
                                 variable_init_kind::normal);
    l.visit_enter_enum_scope();
    l.visit_exit_enum_scope();
    l.visit_variable_declaration(identifier_of(namespace_declaration),
                                 variable_kind::_namespace,
                                 variable_init_kind::normal);
    l.visit_enter_namespace_scope();
    l.visit_exit_namespace_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_multiple_declarations,
     variable_and_namespace_do_not_conflict) {
  const char8 namespace_declaration[] = u8"n";
  const char8 var_declaration[] = u8"n";

  for (variable_kind var_kind :
       {variable_kind::_const, variable_kind::_let, variable_kind::_var}) {
    SCOPED_TRACE(var_kind);

    {
      // namespace n {}
      // var n;
      diag_collector v;
      variable_analyzer l(&v, &default_globals, typescript_var_options);
      l.visit_variable_declaration(identifier_of(namespace_declaration),
                                   variable_kind::_namespace,
                                   variable_init_kind::normal);
      l.visit_enter_namespace_scope();
      l.visit_exit_namespace_scope();
      l.visit_variable_declaration(identifier_of(var_declaration), var_kind,
                                   variable_init_kind::initialized_with_equals);
      l.visit_end_of_module();

      EXPECT_THAT(v.errors, IsEmpty());
    }

    {
      // var n;
      // namespace n {}
      diag_collector v;
      variable_analyzer l(&v, &default_globals, typescript_var_options);
      l.visit_variable_declaration(identifier_of(var_declaration), var_kind,
                                   variable_init_kind::initialized_with_equals);
      l.visit_variable_declaration(identifier_of(namespace_declaration),
                                   variable_kind::_namespace,
                                   variable_init_kind::normal);
      l.visit_enter_namespace_scope();
      l.visit_exit_namespace_scope();
      l.visit_end_of_module();

      EXPECT_THAT(v.errors, IsEmpty());
    }
  }
}

TEST(test_variable_analyzer_multiple_declarations,
     type_alias_and_local_variable_do_not_conflict) {
  const char8 type_declaration[] = u8"x";
  const char8 var_declaration[] = u8"x";

  for (variable_kind var_kind :
       {variable_kind::_const, variable_kind::_let, variable_kind::_var}) {
    SCOPED_TRACE(var_kind);

    {
      // type x = null;
      // var x;
      diag_collector v;
      variable_analyzer l(&v, &default_globals, typescript_var_options);
      l.visit_variable_declaration(identifier_of(type_declaration),
                                   variable_kind::_type_alias,
                                   variable_init_kind::normal);
      l.visit_variable_declaration(identifier_of(var_declaration), var_kind,
                                   variable_init_kind::initialized_with_equals);
      l.visit_end_of_module();

      EXPECT_THAT(v.errors, IsEmpty());
    }

    {
      // var x;
      // type x = null;
      diag_collector v;
      variable_analyzer l(&v, &default_globals, typescript_var_options);
      l.visit_variable_declaration(identifier_of(var_declaration), var_kind,
                                   variable_init_kind::initialized_with_equals);
      l.visit_variable_declaration(identifier_of(type_declaration),
                                   variable_kind::_type_alias,
                                   variable_init_kind::normal);
      l.visit_end_of_module();

      EXPECT_THAT(v.errors, IsEmpty());
    }
  }
}

TEST(test_variable_analyzer_multiple_declarations,
     namespace_can_appear_after_function_or_class_with_same_name) {
  const char8 function_declaration[] = u8"x";
  const char8 class_declaration[] = u8"x";
  const char8 namespace_declaration[] = u8"x";

  {
    // function x() {}
    // namespace x {}
    diag_collector v;
    variable_analyzer l(&v, &default_globals, typescript_var_options);
    l.visit_variable_declaration(identifier_of(function_declaration),
                                 variable_kind::_function,
                                 variable_init_kind::normal);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();

    l.visit_variable_declaration(identifier_of(namespace_declaration),
                                 variable_kind::_namespace,
                                 variable_init_kind::normal);
    l.visit_enter_namespace_scope();
    l.visit_exit_namespace_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // class x {}
    // namespace x {}
    diag_collector v;
    variable_analyzer l(&v, &default_globals, typescript_var_options);
    l.visit_enter_class_scope();
    l.visit_enter_class_scope_body(identifier_of(class_declaration));
    l.visit_exit_class_scope();
    l.visit_variable_declaration(identifier_of(class_declaration),
                                 variable_kind::_class,
                                 variable_init_kind::normal);

    l.visit_variable_declaration(identifier_of(namespace_declaration),
                                 variable_kind::_namespace,
                                 variable_init_kind::normal);
    l.visit_enter_namespace_scope();
    l.visit_exit_namespace_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_multiple_declarations,
     function_or_class_cannot_appear_after_namespace_with_same_name) {
  const char8 function_declaration[] = u8"x";
  const char8 class_declaration[] = u8"x";
  const char8 namespace_declaration[] = u8"x";

  {
    // namespace x {}
    // function x() {}  // ERROR
    diag_collector v;
    variable_analyzer l(&v, &default_globals, typescript_var_options);
    l.visit_variable_declaration(identifier_of(namespace_declaration),
                                 variable_kind::_namespace,
                                 variable_init_kind::normal);
    l.visit_enter_namespace_scope();
    l.visit_exit_namespace_scope();

    l.visit_variable_declaration(identifier_of(function_declaration),
                                 variable_kind::_function,
                                 variable_init_kind::normal);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_SPANS(
                        diag_redeclaration_of_variable,                //
                        redeclaration, span_of(function_declaration),  //
                        original_declaration, span_of(namespace_declaration)),
                }));
  }

  {
    // namespace x {}
    // class x {}      // ERROR
    diag_collector v;
    variable_analyzer l(&v, &default_globals, typescript_var_options);
    l.visit_variable_declaration(identifier_of(namespace_declaration),
                                 variable_kind::_namespace,
                                 variable_init_kind::normal);
    l.visit_enter_namespace_scope();
    l.visit_exit_namespace_scope();

    l.visit_enter_class_scope();
    l.visit_enter_class_scope_body(identifier_of(class_declaration));
    l.visit_exit_class_scope();
    l.visit_variable_declaration(identifier_of(class_declaration),
                                 variable_kind::_class,
                                 variable_init_kind::normal);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_SPANS(
                        diag_redeclaration_of_variable,             //
                        redeclaration, span_of(class_declaration),  //
                        original_declaration, span_of(namespace_declaration)),
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
