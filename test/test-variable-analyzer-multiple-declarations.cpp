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
TEST(Test_Variable_Analyzer_Multiple_Declarations,
     enum_and_namespace_do_not_conflict) {
  const Char8 namespace_declaration[] = u8"A";
  const Char8 enum_declaration[] = u8"A";

  {
    // namespace A {}
    // enum A {}
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_namespace_scope();
    l.visit_exit_namespace_scope();
    l.visit_variable_declaration(identifier_of(namespace_declaration),
                                 Variable_Kind::_namespace,
                                 Variable_Declaration_Flags::none);
    l.visit_variable_declaration(identifier_of(enum_declaration),
                                 Variable_Kind::_enum,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_enum_scope();
    l.visit_exit_enum_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // enum A {}
    // namespace A {}
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(enum_declaration),
                                 Variable_Kind::_enum,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_enum_scope();
    l.visit_exit_enum_scope();
    l.visit_enter_namespace_scope();
    l.visit_exit_namespace_scope();
    l.visit_variable_declaration(identifier_of(namespace_declaration),
                                 Variable_Kind::_namespace,
                                 Variable_Declaration_Flags::none);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     variable_and_namespace_do_not_conflict) {
  const Char8 namespace_declaration[] = u8"n";
  const Char8 var_declaration[] = u8"n";

  for (Variable_Kind var_kind :
       {Variable_Kind::_const, Variable_Kind::_let, Variable_Kind::_var}) {
    SCOPED_TRACE(var_kind);

    {
      // namespace n {}
      // var n;
      Diag_Collector v;
      Variable_Analyzer l(&v, &default_globals, typescript_var_options);
      l.visit_enter_namespace_scope();
      l.visit_exit_namespace_scope();
      l.visit_variable_declaration(identifier_of(namespace_declaration),
                                   Variable_Kind::_namespace,
                                   Variable_Declaration_Flags::none);
      l.visit_variable_declaration(
          identifier_of(var_declaration), var_kind,
          Variable_Declaration_Flags::initialized_with_equals);
      l.visit_end_of_module();

      EXPECT_THAT(v.errors, IsEmpty());
    }

    {
      // var n;
      // namespace n {}
      Diag_Collector v;
      Variable_Analyzer l(&v, &default_globals, typescript_var_options);
      l.visit_variable_declaration(
          identifier_of(var_declaration), var_kind,
          Variable_Declaration_Flags::initialized_with_equals);
      l.visit_enter_namespace_scope();
      l.visit_exit_namespace_scope();
      l.visit_variable_declaration(identifier_of(namespace_declaration),
                                   Variable_Kind::_namespace,
                                   Variable_Declaration_Flags::none);
      l.visit_end_of_module();

      EXPECT_THAT(v.errors, IsEmpty());
    }
  }
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     namespace_can_be_declared_multiple_times) {
  const Char8 namespace_declaration_0[] = u8"ns";
  const Char8 namespace_declaration_1[] = u8"ns";
  const Char8 namespace_declaration_2[] = u8"ns";

  {
    // namespace ns {}
    // namespace ns {}
    // namespace ns {}
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, typescript_var_options);
    l.visit_enter_namespace_scope();
    l.visit_exit_namespace_scope();
    l.visit_variable_declaration(identifier_of(namespace_declaration_0),
                                 Variable_Kind::_namespace,
                                 Variable_Declaration_Flags::none);

    l.visit_enter_namespace_scope();
    l.visit_exit_namespace_scope();
    l.visit_variable_declaration(identifier_of(namespace_declaration_1),
                                 Variable_Kind::_namespace,
                                 Variable_Declaration_Flags::none);

    l.visit_enter_namespace_scope();
    l.visit_exit_namespace_scope();
    l.visit_variable_declaration(identifier_of(namespace_declaration_2),
                                 Variable_Kind::_namespace,
                                 Variable_Declaration_Flags::none);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     type_alias_and_local_variable_do_not_conflict) {
  const Char8 type_declaration[] = u8"x";
  const Char8 var_declaration[] = u8"x";

  for (Variable_Kind var_kind :
       {Variable_Kind::_const, Variable_Kind::_let, Variable_Kind::_var}) {
    SCOPED_TRACE(var_kind);

    {
      // type x = null;
      // var x;
      Diag_Collector v;
      Variable_Analyzer l(&v, &default_globals, typescript_var_options);
      l.visit_variable_declaration(identifier_of(type_declaration),
                                   Variable_Kind::_type_alias,
                                   Variable_Declaration_Flags::none);
      l.visit_variable_declaration(
          identifier_of(var_declaration), var_kind,
          Variable_Declaration_Flags::initialized_with_equals);
      l.visit_end_of_module();

      EXPECT_THAT(v.errors, IsEmpty());
    }

    {
      // var x;
      // type x = null;
      Diag_Collector v;
      Variable_Analyzer l(&v, &default_globals, typescript_var_options);
      l.visit_variable_declaration(
          identifier_of(var_declaration), var_kind,
          Variable_Declaration_Flags::initialized_with_equals);
      l.visit_variable_declaration(identifier_of(type_declaration),
                                   Variable_Kind::_type_alias,
                                   Variable_Declaration_Flags::none);
      l.visit_end_of_module();

      EXPECT_THAT(v.errors, IsEmpty());
    }
  }
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     namespace_can_appear_after_function_or_class_with_same_name) {
  const Char8 function_declaration[] = u8"x";
  const Char8 class_declaration[] = u8"x";
  const Char8 namespace_declaration[] = u8"x";

  {
    // function x() {}
    // namespace x {}
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, typescript_var_options);
    l.visit_variable_declaration(identifier_of(function_declaration),
                                 Variable_Kind::_function,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();

    l.visit_enter_namespace_scope();
    l.visit_exit_namespace_scope();
    l.visit_variable_declaration(identifier_of(namespace_declaration),
                                 Variable_Kind::_namespace,
                                 Variable_Declaration_Flags::none);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // class x {}
    // namespace x {}
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, typescript_var_options);
    l.visit_enter_class_scope();
    l.visit_enter_class_scope_body(identifier_of(class_declaration));
    l.visit_exit_class_scope();
    l.visit_variable_declaration(identifier_of(class_declaration),
                                 Variable_Kind::_class,
                                 Variable_Declaration_Flags::none);

    l.visit_enter_namespace_scope();
    l.visit_exit_namespace_scope();
    l.visit_variable_declaration(identifier_of(namespace_declaration),
                                 Variable_Kind::_namespace,
                                 Variable_Declaration_Flags::none);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     function_or_class_cannot_appear_after_non_empty_namespace_with_same_name) {
  const Char8 function_declaration[] = u8"x";
  const Char8 class_declaration[] = u8"x";
  const Char8 namespace_declaration[] = u8"x";

  {
    // namespace x { ; }
    // function x() {}  // ERROR
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, typescript_var_options);
    l.visit_enter_namespace_scope();
    l.visit_exit_namespace_scope();
    l.visit_variable_declaration(
        identifier_of(namespace_declaration), Variable_Kind::_namespace,
        Variable_Declaration_Flags::non_empty_namespace);

    l.visit_variable_declaration(identifier_of(function_declaration),
                                 Variable_Kind::_function,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_SPANS(
                        Diag_Redeclaration_Of_Variable,                //
                        redeclaration, span_of(function_declaration),  //
                        original_declaration, span_of(namespace_declaration)),
                }));
  }

  {
    // namespace x { ; }
    // class x {}      // ERROR
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, typescript_var_options);
    l.visit_enter_namespace_scope();
    l.visit_exit_namespace_scope();
    l.visit_variable_declaration(
        identifier_of(namespace_declaration), Variable_Kind::_namespace,
        Variable_Declaration_Flags::non_empty_namespace);

    l.visit_enter_class_scope();
    l.visit_enter_class_scope_body(identifier_of(class_declaration));
    l.visit_exit_class_scope();
    l.visit_variable_declaration(identifier_of(class_declaration),
                                 Variable_Kind::_class,
                                 Variable_Declaration_Flags::none);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_SPANS(
                        Diag_Redeclaration_Of_Variable,             //
                        redeclaration, span_of(class_declaration),  //
                        original_declaration, span_of(namespace_declaration)),
                }));
  }
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     function_or_class_can_appear_after_empty_namespace_with_same_name) {
  const Char8 function_declaration[] = u8"x";
  const Char8 class_declaration[] = u8"x";
  const Char8 namespace_declaration[] = u8"x";

  {
    // namespace x {}
    // function x() {}
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, typescript_var_options);
    l.visit_enter_namespace_scope();
    l.visit_exit_namespace_scope();
    l.visit_variable_declaration(identifier_of(namespace_declaration),
                                 Variable_Kind::_namespace,
                                 Variable_Declaration_Flags::none);

    l.visit_variable_declaration(identifier_of(function_declaration),
                                 Variable_Kind::_function,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // namespace x {}
    // class x {}
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, typescript_var_options);
    l.visit_enter_namespace_scope();
    l.visit_exit_namespace_scope();
    l.visit_variable_declaration(identifier_of(namespace_declaration),
                                 Variable_Kind::_namespace,
                                 Variable_Declaration_Flags::none);

    l.visit_enter_class_scope();
    l.visit_enter_class_scope_body(identifier_of(class_declaration));
    l.visit_exit_class_scope();
    l.visit_variable_declaration(identifier_of(class_declaration),
                                 Variable_Kind::_class,
                                 Variable_Declaration_Flags::none);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     function_parameter_can_have_same_name_as_generic_parameter) {
  const Char8 function_parameter_declaration[] = u8"T";
  const Char8 type_parameter_declaration[] = u8"T";

  {
    // (function <T>(T) {});
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, typescript_var_options);
    l.visit_enter_function_scope();
    l.visit_variable_declaration(identifier_of(type_parameter_declaration),
                                 Variable_Kind::_generic_parameter,
                                 Variable_Declaration_Flags::none);
    l.visit_variable_declaration(identifier_of(function_parameter_declaration),
                                 Variable_Kind::_function_parameter,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
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
