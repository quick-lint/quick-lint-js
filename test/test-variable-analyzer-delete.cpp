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
TEST(Test_Variable_Analyzer_Delete_JavaScript,
     deleting_local_variable_is_a_warning) {
  const Char8 declaration[] = u8"v";
  Padded_String delete_expression(u8"delete v"_sv);
  Source_Code_Span delete_keyword_span(delete_expression.data(),
                                       delete_expression.data() + 6);
  ASSERT_EQ(delete_keyword_span.string_view(), u8"delete"_sv);
  Source_Code_Span deleted_variable_span(delete_expression.data() + 7,
                                         delete_expression.data() + 8);
  ASSERT_EQ(deleted_variable_span.string_view(), u8"v"_sv);

  {
    // (() => {
    //   let v;
    //   delete v;
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(declaration),
                                 Variable_Kind::_let,
                                 Variable_Declaration_Flags::none);
    l.visit_variable_delete_use(Identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_FIELD(
                Diag_Redundant_Delete_Statement_On_Variable, delete_expression,
                Offsets_Matcher(&delete_expression, 0, u8"delete x"_sv)),
        }));
  }

  {
    // (() => {
    //   let v;
    //   {
    //     delete v;
    //   }
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(declaration),
                                 Variable_Kind::_let,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_block_scope();
    l.visit_variable_delete_use(Identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_exit_block_scope();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_FIELD(
                Diag_Redundant_Delete_Statement_On_Variable, delete_expression,
                Offsets_Matcher(&delete_expression, 0, u8"delete x"_sv)),
        }));
  }

  {
    // (() => {
    //   let v;
    //   (() => {
    //     delete v;
    //   });
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(declaration),
                                 Variable_Kind::_let,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_delete_use(Identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_exit_function_scope();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_FIELD(
                Diag_Redundant_Delete_Statement_On_Variable, delete_expression,
                Offsets_Matcher(&delete_expression, 0, u8"delete x"_sv)),
        }));
  }

  {
    // (() => {
    //   (() => {
    //     delete v;
    //   });
    //   let v;
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_delete_use(Identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_exit_function_scope();
    l.visit_variable_declaration(identifier_of(declaration),
                                 Variable_Kind::_let,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_FIELD(
                Diag_Redundant_Delete_Statement_On_Variable, delete_expression,
                Offsets_Matcher(&delete_expression, 0, u8"delete x"_sv)),
        }));
  }

  {
    // (() => {
    //   let v;
    //   (() => {
    //     (() => {
    //       delete v;
    //     });
    //   });
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(declaration),
                                 Variable_Kind::_let,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_delete_use(Identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_exit_function_scope();
    l.visit_exit_function_scope();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_FIELD(
                Diag_Redundant_Delete_Statement_On_Variable, delete_expression,
                Offsets_Matcher(&delete_expression, 0, u8"delete x"_sv)),
        }));
  }
}

TEST(Test_Variable_Analyzer_Delete_JavaScript,
     deleting_local_variable_declared_later_is_a_warning) {
  const Char8 declaration[] = u8"v";
  Padded_String delete_expression(u8"delete v"_sv);
  Source_Code_Span delete_keyword_span(delete_expression.data(),
                                       delete_expression.data() + 6);
  ASSERT_EQ(delete_keyword_span.string_view(), u8"delete"_sv);
  Source_Code_Span deleted_variable_span(delete_expression.data() + 7,
                                         delete_expression.data() + 8);
  ASSERT_EQ(deleted_variable_span.string_view(), u8"v"_sv);

  // (() => {
  //   delete v;
  //   let v;
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_delete_use(Identifier(deleted_variable_span),
                              delete_keyword_span);
  l.visit_variable_declaration(identifier_of(declaration), Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_FIELD(
              Diag_Redundant_Delete_Statement_On_Variable, delete_expression,
              Offsets_Matcher(&delete_expression, 0, u8"delete x"_sv)),
      }));
}

TEST(Test_Variable_Analyzer_Delete_JavaScript,
     deleting_declared_module_variable_is_a_warning) {
  const Char8 declaration[] = u8"v";
  Padded_String delete_expression(u8"delete v"_sv);
  Source_Code_Span delete_keyword_span(delete_expression.data(),
                                       delete_expression.data() + 6);
  ASSERT_EQ(delete_keyword_span.string_view(), u8"delete"_sv);
  Source_Code_Span deleted_variable_span(delete_expression.data() + 7,
                                         delete_expression.data() + 8);
  ASSERT_EQ(deleted_variable_span.string_view(), u8"v"_sv);

  // let v;
  // delete v;
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(declaration), Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_variable_delete_use(Identifier(deleted_variable_span),
                              delete_keyword_span);
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_FIELD(
              Diag_Redundant_Delete_Statement_On_Variable, delete_expression,
              Offsets_Matcher(&delete_expression, 0, u8"delete x"_sv)),
      }));
}

TEST(Test_Variable_Analyzer_Delete_JavaScript,
     deleting_declared_global_variable_is_ok) {
  Padded_String code(u8"delete myGlobalVariable"_sv);
  Source_Code_Span delete_keyword_span(code.data(), code.data() + 6);
  ASSERT_EQ(delete_keyword_span.string_view(), u8"delete"_sv);
  Source_Code_Span deleted_variable_span(code.data() + 7, code.data() + 23);
  ASSERT_EQ(deleted_variable_span.string_view(), u8"myGlobalVariable"_sv);

  Global_Declared_Variable_Set globals;
  globals.add_global_variable(Global_Declared_Variable{
      .name = u8"myGlobalVariable"_sv,
      .is_writable = true,
      .is_shadowable = true,
      .is_type_only = false,
  });

  {
    // delete myGlobalVariable;
    Diag_Collector v;
    Variable_Analyzer l(&v, &globals, javascript_var_options);
    l.visit_variable_delete_use(Identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // (() => {
    //   delete myGlobalVariable;
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_delete_use(Identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer_Delete_JavaScript,
     deleting_undeclared_global_variable_is_ok) {
  Padded_String code(u8"delete myGlobalVariable"_sv);
  Source_Code_Span delete_keyword_span(code.data(), code.data() + 6);
  ASSERT_EQ(delete_keyword_span.string_view(), u8"delete"_sv);
  Source_Code_Span deleted_variable_span(code.data() + 7, code.data() + 23);
  ASSERT_EQ(deleted_variable_span.string_view(), u8"myGlobalVariable"_sv);

  {
    // delete myGlobalVariable;
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_delete_use(Identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // (() => {
    //   delete myGlobalVariable;
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_delete_use(Identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer_Delete_TypeScript,
     deleting_local_variable_is_an_error) {
  const Char8 my_var_declaration[] = u8"myVar";
  Padded_String delete_expression(u8"delete myVar"_sv);
  Source_Code_Span delete_keyword_span(
      delete_expression.data(),
      delete_expression.data() + u8"delete"_sv.size());
  ASSERT_EQ(delete_keyword_span.string_view(), u8"delete"_sv);
  Source_Code_Span deleted_variable_span(
      delete_expression.data() + u8"delete "_sv.size(),
      delete_expression.cend());
  ASSERT_EQ(deleted_variable_span.string_view(), u8"myVar"_sv);

  {
    // let myVar;
    // delete myVar;  // ERROR
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, typescript_var_options);
    l.visit_variable_declaration(identifier_of(my_var_declaration),
                                 Variable_Kind::_let,
                                 Variable_Declaration_Flags::none);
    l.visit_variable_delete_use(Identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_FIELD(
                Diag_TypeScript_Delete_Cannot_Delete_Variables,
                delete_expression,
                Offsets_Matcher(&delete_expression, 0, u8"delete myVar"_sv)),
        }));
  }

  {
    // delete myVar;  // ERROR
    // let myVar;
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, typescript_var_options);
    l.visit_variable_delete_use(Identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_variable_declaration(identifier_of(my_var_declaration),
                                 Variable_Kind::_let,
                                 Variable_Declaration_Flags::none);
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_FIELD(
                Diag_TypeScript_Delete_Cannot_Delete_Variables,
                delete_expression,
                Offsets_Matcher(&delete_expression, 0, u8"delete myVar"_sv)),
        }));
  }
}

TEST(Test_Variable_Analyzer_Delete_TypeScript,
     deleting_global_variable_is_an_error) {
  Padded_String delete_expression(u8"delete myGlobalVariable"_sv);
  Source_Code_Span delete_keyword_span(
      delete_expression.data(),
      delete_expression.data() + u8"delete"_sv.size());
  ASSERT_EQ(delete_keyword_span.string_view(), u8"delete"_sv);
  Source_Code_Span deleted_variable_span(
      delete_expression.data() + u8"delete "_sv.size(),
      delete_expression.cend());
  ASSERT_EQ(deleted_variable_span.string_view(), u8"myGlobalVariable"_sv);

  {
    // delete myGlobalVariable;  // ERROR
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, typescript_var_options);
    l.visit_variable_delete_use(Identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_FIELD(Diag_TypeScript_Delete_Cannot_Delete_Variables,
                            delete_expression,
                            Offsets_Matcher(&delete_expression, 0,
                                            u8"delete myGlobalVariable"_sv)),
        }));
  }

  {
    // delete myGlobalVariable;  // ERROR
    Global_Declared_Variable_Set globals;
    globals.add_global_variable(Global_Declared_Variable{
        .name = u8"myGlobalVariable"_sv,
        .is_writable = true,
        .is_shadowable = true,
        .is_type_only = false,
    });
    Diag_Collector v;
    Variable_Analyzer l(&v, &globals, typescript_var_options);
    l.visit_variable_delete_use(Identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_FIELD(Diag_TypeScript_Delete_Cannot_Delete_Variables,
                            delete_expression,
                            Offsets_Matcher(&delete_expression, 0,
                                            u8"delete myGlobalVariable"_sv)),
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
