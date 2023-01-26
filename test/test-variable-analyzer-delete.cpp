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
TEST(test_variable_analyzer_delete_javascript,
     deleting_local_variable_is_a_warning) {
  const char8 declaration[] = u8"v";
  padded_string delete_expression(u8"delete v"_sv);
  source_code_span delete_keyword_span(delete_expression.data(),
                                       delete_expression.data() + 6);
  ASSERT_EQ(delete_keyword_span.string_view(), u8"delete"_sv);
  source_code_span deleted_variable_span(delete_expression.data() + 7,
                                         delete_expression.data() + 8);
  ASSERT_EQ(deleted_variable_span.string_view(), u8"v"_sv);

  {
    // (() => {
    //   let v;
    //   delete v;
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_let,
                                 variable_init_kind::normal);
    l.visit_variable_delete_use(identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_FIELD(
                diag_redundant_delete_statement_on_variable, delete_expression,
                offsets_matcher(&delete_expression, 0, u8"delete x"_sv)),
        }));
  }

  {
    // (() => {
    //   let v;
    //   {
    //     delete v;
    //   }
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_let,
                                 variable_init_kind::normal);
    l.visit_enter_block_scope();
    l.visit_variable_delete_use(identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_exit_block_scope();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_FIELD(
                diag_redundant_delete_statement_on_variable, delete_expression,
                offsets_matcher(&delete_expression, 0, u8"delete x"_sv)),
        }));
  }

  {
    // (() => {
    //   let v;
    //   (() => {
    //     delete v;
    //   });
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_let,
                                 variable_init_kind::normal);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_delete_use(identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_exit_function_scope();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_FIELD(
                diag_redundant_delete_statement_on_variable, delete_expression,
                offsets_matcher(&delete_expression, 0, u8"delete x"_sv)),
        }));
  }

  {
    // (() => {
    //   (() => {
    //     delete v;
    //   });
    //   let v;
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_delete_use(identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_exit_function_scope();
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_let,
                                 variable_init_kind::normal);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_FIELD(
                diag_redundant_delete_statement_on_variable, delete_expression,
                offsets_matcher(&delete_expression, 0, u8"delete x"_sv)),
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
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_let,
                                 variable_init_kind::normal);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_delete_use(identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_exit_function_scope();
    l.visit_exit_function_scope();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_FIELD(
                diag_redundant_delete_statement_on_variable, delete_expression,
                offsets_matcher(&delete_expression, 0, u8"delete x"_sv)),
        }));
  }
}

TEST(test_variable_analyzer_delete_javascript,
     deleting_local_variable_declared_later_is_a_warning) {
  const char8 declaration[] = u8"v";
  padded_string delete_expression(u8"delete v"_sv);
  source_code_span delete_keyword_span(delete_expression.data(),
                                       delete_expression.data() + 6);
  ASSERT_EQ(delete_keyword_span.string_view(), u8"delete"_sv);
  source_code_span deleted_variable_span(delete_expression.data() + 7,
                                         delete_expression.data() + 8);
  ASSERT_EQ(deleted_variable_span.string_view(), u8"v"_sv);

  // (() => {
  //   delete v;
  //   let v;
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_delete_use(identifier(deleted_variable_span),
                              delete_keyword_span);
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let,
                               variable_init_kind::normal);
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_FIELD(
              diag_redundant_delete_statement_on_variable, delete_expression,
              offsets_matcher(&delete_expression, 0, u8"delete x"_sv)),
      }));
}

TEST(test_variable_analyzer_delete_javascript,
     deleting_declared_module_variable_is_a_warning) {
  const char8 declaration[] = u8"v";
  padded_string delete_expression(u8"delete v"_sv);
  source_code_span delete_keyword_span(delete_expression.data(),
                                       delete_expression.data() + 6);
  ASSERT_EQ(delete_keyword_span.string_view(), u8"delete"_sv);
  source_code_span deleted_variable_span(delete_expression.data() + 7,
                                         delete_expression.data() + 8);
  ASSERT_EQ(deleted_variable_span.string_view(), u8"v"_sv);

  // let v;
  // delete v;
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let,
                               variable_init_kind::normal);
  l.visit_variable_delete_use(identifier(deleted_variable_span),
                              delete_keyword_span);
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_FIELD(
              diag_redundant_delete_statement_on_variable, delete_expression,
              offsets_matcher(&delete_expression, 0, u8"delete x"_sv)),
      }));
}

TEST(test_variable_analyzer_delete_javascript,
     deleting_declared_global_variable_is_ok) {
  padded_string code(u8"delete myGlobalVariable"_sv);
  source_code_span delete_keyword_span(code.data(), code.data() + 6);
  ASSERT_EQ(delete_keyword_span.string_view(), u8"delete"_sv);
  source_code_span deleted_variable_span(code.data() + 7, code.data() + 23);
  ASSERT_EQ(deleted_variable_span.string_view(), u8"myGlobalVariable"_sv);

  global_declared_variable_set globals;
  globals.add_global_variable(global_declared_variable{
      .name = u8"myGlobalVariable"_sv,
      .is_writable = true,
      .is_shadowable = true,
  });

  {
    // delete myGlobalVariable;
    diag_collector v;
    variable_analyzer l(&v, &globals, javascript_var_options);
    l.visit_variable_delete_use(identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // (() => {
    //   delete myGlobalVariable;
    // });
    diag_collector v;
    variable_analyzer l(&v, &globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_delete_use(identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_delete_javascript,
     deleting_undeclared_global_variable_is_ok) {
  padded_string code(u8"delete myGlobalVariable"_sv);
  source_code_span delete_keyword_span(code.data(), code.data() + 6);
  ASSERT_EQ(delete_keyword_span.string_view(), u8"delete"_sv);
  source_code_span deleted_variable_span(code.data() + 7, code.data() + 23);
  ASSERT_EQ(deleted_variable_span.string_view(), u8"myGlobalVariable"_sv);

  {
    // delete myGlobalVariable;
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_delete_use(identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // (() => {
    //   delete myGlobalVariable;
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_delete_use(identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_delete_typescript,
     deleting_local_variable_is_an_error) {
  const char8 my_var_declaration[] = u8"myVar";
  padded_string delete_expression(u8"delete myVar"_sv);
  source_code_span delete_keyword_span(
      delete_expression.data(), delete_expression.data() + strlen(u8"delete"));
  ASSERT_EQ(delete_keyword_span.string_view(), u8"delete"_sv);
  source_code_span deleted_variable_span(
      delete_expression.data() + strlen(u8"delete "), delete_expression.cend());
  ASSERT_EQ(deleted_variable_span.string_view(), u8"myVar"_sv);

  {
    // let myVar;
    // delete myVar;  // ERROR
    diag_collector v;
    variable_analyzer l(&v, &default_globals, typescript_var_options);
    l.visit_variable_declaration(identifier_of(my_var_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::normal);
    l.visit_variable_delete_use(identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_FIELD(
                diag_typescript_delete_cannot_delete_variables,
                delete_expression,
                offsets_matcher(&delete_expression, 0, u8"delete myVar"_sv)),
        }));
  }

  {
    // delete myVar;  // ERROR
    // let myVar;
    diag_collector v;
    variable_analyzer l(&v, &default_globals, typescript_var_options);
    l.visit_variable_delete_use(identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_variable_declaration(identifier_of(my_var_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::normal);
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_FIELD(
                diag_typescript_delete_cannot_delete_variables,
                delete_expression,
                offsets_matcher(&delete_expression, 0, u8"delete myVar"_sv)),
        }));
  }
}

TEST(test_variable_analyzer_delete_typescript,
     deleting_global_variable_is_an_error) {
  padded_string delete_expression(u8"delete myGlobalVariable"_sv);
  source_code_span delete_keyword_span(
      delete_expression.data(), delete_expression.data() + strlen(u8"delete"));
  ASSERT_EQ(delete_keyword_span.string_view(), u8"delete"_sv);
  source_code_span deleted_variable_span(
      delete_expression.data() + strlen(u8"delete "), delete_expression.cend());
  ASSERT_EQ(deleted_variable_span.string_view(), u8"myGlobalVariable"_sv);

  {
    // delete myGlobalVariable;  // ERROR
    diag_collector v;
    variable_analyzer l(&v, &default_globals, typescript_var_options);
    l.visit_variable_delete_use(identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_FIELD(diag_typescript_delete_cannot_delete_variables,
                            delete_expression,
                            offsets_matcher(&delete_expression, 0,
                                            u8"delete myGlobalVariable"_sv)),
        }));
  }

  {
    // delete myGlobalVariable;  // ERROR
    global_declared_variable_set globals;
    globals.add_global_variable(global_declared_variable{
        .name = u8"myGlobalVariable"_sv,
        .is_writable = true,
        .is_shadowable = true,
    });
    diag_collector v;
    variable_analyzer l(&v, &globals, typescript_var_options);
    l.visit_variable_delete_use(identifier(deleted_variable_span),
                                delete_keyword_span);
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_FIELD(diag_typescript_delete_cannot_delete_variables,
                            delete_expression,
                            offsets_matcher(&delete_expression, 0,
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
