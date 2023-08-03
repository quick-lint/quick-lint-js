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
TEST(Test_Variable_Analyzer_Namespace, empty_namespace) {
  test_parse_and_analyze(u8"namespace NS { } "_sv, no_diags,
                         typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Namespace,
     namespace_name_is_visible_outside_namespace) {
  test_parse_and_analyze(
      u8"namespace NS { } "_sv
      u8"NS;"_sv,
      no_diags, typescript_analyze_options, default_globals);
}

// TODO(strager): Is this correct? TypeScript's compiler (as of v4.8.2)
// complains about accessing members of the namespace, but doesn't complain
// about referencing the namespace itself.
TEST(Test_Variable_Analyzer_Namespace,
     namespace_name_is_usable_before_namespace) {
  test_parse_and_analyze(
      u8"NS;"_sv
      u8"namespace NS { } "_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Namespace,
     namespace_name_is_visible_inside_namespace) {
  test_parse_and_analyze(
      u8"namespace NS {"_sv
      u8"  NS;"_sv
      u8"} "_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Namespace,
     variables_declared_inside_namespace_are_not_accessible_outside) {
  test_parse_and_analyze(
      u8"namespace NS { export class C {}  }  C;"_sv,
      u8"                                     ^ Diag_Use_Of_Undeclared_Variable.name"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"namespace NS { export var v; }  v;"_sv,
      u8"                                ^ Diag_Use_Of_Undeclared_Variable.name"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Namespace,
     uses_in_namespace_might_refer_to_symbols_in_other_files) {
  const Char8 namespace_declaration[] = u8"NS";
  const Char8 variable_use[] = u8"myVar";
  const Char8 assignment[] = u8"myVarWithAssignment";
  const Char8 typeof_use[] = u8"myVarWithTypeof";
  const Char8 type_use[] = u8"MyType";

  static const Padded_String delete_expression(u8"delete myVarWithDelete"_sv);
  static const Source_Code_Span delete_keyword_span(
      delete_expression.data(),
      delete_expression.data() + u8"delete"_sv.size());
  ASSERT_EQ(delete_keyword_span.string_view(), u8"delete"_sv);
  static const Source_Code_Span deleted_variable_span(
      delete_expression.data() + u8"delete "_sv.size(),
      delete_expression.cend());
  ASSERT_EQ(deleted_variable_span.string_view(), u8"myVarWithDelete"_sv);

  {
    // namespace NS {
    //   myVar;                    // visit_variable_use
    //   myVarWithAssignment = 0;  // visit_variable_assignment
    //   delete myVarWithDelete;   // visit_variable_delete_use
    //   typeof myVarWithTypeof;   // visit_variable_typeof_use
    //   null as MyType;           // visit_variable_type_use
    // }
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_namespace_scope();
    l.visit_variable_use(identifier_of(variable_use));
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_variable_delete_use(Identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_variable_typeof_use(identifier_of(typeof_use));
    l.visit_variable_type_use(identifier_of(type_use));
    l.visit_exit_namespace_scope();
    l.visit_variable_declaration(identifier_of(namespace_declaration),
                                 Variable_Kind::_namespace,
                                 Variable_Declaration_Flags::none);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer_Namespace,
     eval_in_namespace_cannot_declare_variables_outside_namespace) {
  const Char8 namespace_declaration[] = u8"NS";
  const Char8 eval_use[] = u8"eval";
  const Char8 variable_use[] = u8"myVar";

  {
    // namespace NS {
    //   eval("let myVar");
    // }
    // myVar;  // ERROR
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_namespace_scope();
    l.visit_variable_use(identifier_of(eval_use));
    l.visit_exit_namespace_scope();
    l.visit_variable_declaration(identifier_of(namespace_declaration),
                                 Variable_Kind::_namespace,
                                 Variable_Declaration_Flags::none);
    l.visit_variable_use(identifier_of(variable_use));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAreArray({
                              DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Variable,
                                             name, span_of(variable_use)),
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
