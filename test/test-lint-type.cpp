// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/identifier-support.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/lint.h>

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;

namespace quick_lint_js {
namespace {
global_declared_variable_set default_globals = configuration().globals();

TEST(test_lint_type, type_use_after_declaration_is_okay) {
  const char8 declaration[] = u8"I";
  const char8 use[] = u8"I";

  for (variable_kind kind :
       {variable_kind::_class, variable_kind::_interface}) {
    SCOPED_TRACE(kind);

    // interface I {}
    // ({}) as I;
    diag_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(declaration), kind,
                                 variable_init_kind::normal);
    l.visit_variable_type_use(identifier_of(use));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint_type, type_use_in_block_scope_after_declaration_is_okay) {
  const char8 declaration[] = u8"I";
  const char8 use[] = u8"I";

  for (variable_kind kind :
       {variable_kind::_class, variable_kind::_interface}) {
    SCOPED_TRACE(kind);

    // interface I {}
    // {
    //   ({}) as I;
    // }
    diag_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(declaration), kind,
                                 variable_init_kind::normal);
    l.visit_enter_block_scope();
    l.visit_variable_type_use(identifier_of(use));
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint_type, type_use_with_no_declaration_is_an_error) {
  const char8 use[] = u8"C";

  // ({}) as C;  // ERROR
  diag_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_type_use(identifier_of(use));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_FIELD(diag_use_of_undeclared_type,
                                                    name, span_matcher(use))));
}

TEST(test_lint_type, type_use_after_declaration_in_block_scope_is_an_error) {
  const char8 declaration[] = u8"I";
  const char8 use[] = u8"I";

  for (variable_kind kind :
       {variable_kind::_class, variable_kind::_interface}) {
    SCOPED_TRACE(kind);

    // {
    //   interface I {}
    // }
    // ({}) as I;
    diag_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(declaration), kind,
                                 variable_init_kind::normal);
    l.visit_exit_block_scope();
    l.visit_variable_type_use(identifier_of(use));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_FIELD(diag_use_of_undeclared_type, name,
                                            span_matcher(use))));
  }
}

TEST(test_lint_type, type_use_before_declaration_is_okay) {
  const char8 declaration[] = u8"I";
  const char8 use[] = u8"I";

  for (variable_kind kind :
       {variable_kind::_class, variable_kind::_interface}) {
    SCOPED_TRACE(kind);

    // ({}) as I;
    // interface I {}
    diag_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_type_use(identifier_of(use));
    l.visit_variable_declaration(identifier_of(declaration), kind,
                                 variable_init_kind::normal);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint_type, type_use_of_import_is_okay) {
  const char8 declaration[] = u8"I";
  const char8 use[] = u8"I";

  {
    // ({}) as I;
    // import {I} from "module";
    diag_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_type_use(identifier_of(use));
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_import,
                                 variable_init_kind::normal);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // import {I} from "module";
    // ({}) as I;
    diag_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_import,
                                 variable_init_kind::normal);
    l.visit_variable_type_use(identifier_of(use));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint_type, type_use_does_not_see_non_type_variables) {
  const char8 declaration[] = u8"I";
  const char8 use[] = u8"I";

  for (variable_kind kind : {
           variable_kind::_catch,
           variable_kind::_const,
           variable_kind::_function,
           variable_kind::_let,
           variable_kind::_parameter,
           variable_kind::_var,
       }) {
    SCOPED_TRACE(kind);

    {
      // let I;
      // ({}) as I;
      diag_collector v;
      linter l(&v, &default_globals);
      l.visit_variable_declaration(identifier_of(declaration), kind,
                                   variable_init_kind::normal);
      l.visit_variable_type_use(identifier_of(use));
      l.visit_end_of_module();

      // TODO(strager): Report a more helpful message indicating that 'I' is a
      // function or variable, not a type.
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_FIELD(diag_use_of_undeclared_type, name,
                                              span_matcher(use))));
    }

    {
      // let I;
      // {
      //   ({}) as I;
      // }
      diag_collector v;
      linter l(&v, &default_globals);
      l.visit_variable_declaration(identifier_of(declaration), kind,
                                   variable_init_kind::normal);
      l.visit_enter_block_scope();
      l.visit_variable_type_use(identifier_of(use));
      l.visit_exit_block_scope();
      l.visit_end_of_module();

      // TODO(strager): Report a more helpful message indicating that 'I' is a
      // function or variable, not a type.
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_FIELD(diag_use_of_undeclared_type, name,
                                              span_matcher(use))));
    }

    {
      // let I;
      // (() => {
      //   ({}) as I;
      // });
      diag_collector v;
      linter l(&v, &default_globals);
      l.visit_variable_declaration(identifier_of(declaration), kind,
                                   variable_init_kind::normal);
      l.visit_enter_function_scope();
      l.visit_enter_function_scope_body();
      l.visit_variable_type_use(identifier_of(use));
      l.visit_exit_function_scope();
      l.visit_end_of_module();

      // TODO(strager): Report a more helpful message indicating that 'I' is a
      // function or variable, not a type.
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_FIELD(diag_use_of_undeclared_type, name,
                                              span_matcher(use))));
    }

    {
      // (() => {
      //   (() => {
      //     ({}) as I;
      //   });
      // });
      // let I;
      diag_collector v;
      linter l(&v, &default_globals);
      l.visit_variable_declaration(identifier_of(declaration), kind,
                                   variable_init_kind::normal);
      l.visit_enter_function_scope();
      l.visit_enter_function_scope_body();
      l.visit_enter_function_scope();
      l.visit_enter_function_scope_body();
      l.visit_variable_type_use(identifier_of(use));
      l.visit_exit_function_scope();
      l.visit_exit_function_scope();
      l.visit_end_of_module();

      // TODO(strager): Report a more helpful message indicating that 'I' is a
      // function or variable, not a type.
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_FIELD(diag_use_of_undeclared_type, name,
                                              span_matcher(use))));
    }
  }
}

TEST(test_lint_type, interfaces_are_ignored_in_runtime_expressions) {
  using diags_matcher =
      testing::Matcher<const std::vector<diag_collector::diag>&>;

  static const char8 outer_declaration[] = u8"I";
  static const char8 declaration[] = u8"I";

  static const char8 assignment[] = u8"I";
  static const char8 use[] = u8"I";

  static const padded_string delete_expression(u8"delete I"_sv);
  static const source_code_span delete_keyword_span(
      delete_expression.data(), delete_expression.data() + 6);
  ASSERT_EQ(delete_keyword_span.string_view(), u8"delete"_sv);
  static const source_code_span deleted_variable_span(
      delete_expression.data() + 7, delete_expression.data() + 8);
  ASSERT_EQ(deleted_variable_span.string_view(), u8"I"_sv);

  struct variable_visit_kind {
    const char* description;
    void (*visit)(linter&);

    // Used when no run-time variable exists with the same name as the
    // If a run-time variable exists with the same name as the interface,
    // 'runtime_var_kind' is set to that variable's kind.
    //
    // If no run-time variable exists with the same name as the interface,
    // 'runtime_var_kind' is nullopt.
    diags_matcher (*get_diags_matcher)(
        std::optional<variable_kind> runtime_var_kind);
  };

  variable_visit_kind variable_visit_kinds[] = {
      {
          .description = "visit_variable_assignment",
          .visit =
              [](linter& l) {
                l.visit_variable_assignment(identifier_of(assignment));
              },
          .get_diags_matcher = [](std::optional<variable_kind> runtime_var_kind)
              -> diags_matcher {
            if (runtime_var_kind.has_value()) {
              if (*runtime_var_kind == variable_kind::_const) {
                return ElementsAre(DIAG_TYPE_2_FIELDS(
                    diag_assignment_to_const_variable,     //
                    assignment, span_matcher(assignment),  //
                    declaration, span_matcher(outer_declaration)));
              } else {
                return IsEmpty();
              }
            } else {
              // TODO(strager): Report a more helpful message.
              return ElementsAre(
                  DIAG_TYPE_FIELD(diag_assignment_to_undeclared_variable,
                                  assignment, span_matcher(assignment)));
            }
          },
      },

      {
          .description = "visit_variable_delete_use",
          .visit =
              [](linter& l) {
                l.visit_variable_delete_use(identifier(deleted_variable_span),
                                            delete_keyword_span);
              },
          .get_diags_matcher = [](std::optional<variable_kind> runtime_var_kind)
              -> diags_matcher {
            if (runtime_var_kind.has_value()) {
              return ElementsAre(DIAG_TYPE_FIELD(
                  diag_redundant_delete_statement_on_variable,
                  delete_expression,
                  offsets_matcher(&delete_expression, 0, u8"delete I")));
            } else {
              return IsEmpty();
            }
          },
      },

      {.description = "visit_variable_use",
       .visit = [](linter& l) { l.visit_variable_use(identifier_of(use)); },
       .get_diags_matcher =
           [](std::optional<variable_kind> runtime_var_kind) -> diags_matcher {
         if (runtime_var_kind.has_value()) {
           return IsEmpty();
         } else {
           // TODO(strager): Report a more helpful message.
           return ElementsAre(DIAG_TYPE_FIELD(diag_use_of_undeclared_variable,
                                              name, span_matcher(use)));
         }
       }},
  };

  for (variable_visit_kind& visit_kind : variable_visit_kinds) {
    SCOPED_TRACE(visit_kind.description);

    {
      // interface I {}
      // I;              // ERROR
      diag_collector v;
      linter l(&v, &default_globals);
      l.visit_variable_declaration(identifier_of(declaration),
                                   variable_kind::_interface,
                                   variable_init_kind::normal);
      visit_kind.visit(l);
      l.visit_end_of_module();

      EXPECT_THAT(v.errors, visit_kind.get_diags_matcher(std::nullopt));
    }

    {
      // interface I {}
      // {
      //   I;            // ERROR
      // }
      diag_collector v;
      linter l(&v, &default_globals);
      l.visit_variable_declaration(identifier_of(declaration),
                                   variable_kind::_interface,
                                   variable_init_kind::normal);
      l.visit_enter_block_scope();
      visit_kind.visit(l);
      l.visit_exit_block_scope();
      l.visit_end_of_module();

      EXPECT_THAT(v.errors, visit_kind.get_diags_matcher(std::nullopt));
    }

    for (variable_kind outer_kind : {
             variable_kind::_catch,
             variable_kind::_const,
             variable_kind::_function,
             variable_kind::_let,
             variable_kind::_parameter,
             variable_kind::_var,
         }) {
      SCOPED_TRACE(outer_kind);

      {
        // let I;
        // {
        //   interface I {}
        //   I;
        // }
        diag_collector v;
        linter l(&v, &default_globals);
        l.visit_variable_declaration(identifier_of(outer_declaration),
                                     outer_kind, variable_init_kind::normal);
        l.visit_enter_block_scope();
        l.visit_variable_declaration(identifier_of(declaration),
                                     variable_kind::_interface,
                                     variable_init_kind::normal);
        visit_kind.visit(l);
        l.visit_exit_block_scope();
        l.visit_end_of_module();

        EXPECT_THAT(v.errors, visit_kind.get_diags_matcher(outer_kind));
      }

      {
        // let I;
        // interface I {}
        // {
        //   I;
        // }
        diag_collector v;
        linter l(&v, &default_globals);
        l.visit_variable_declaration(identifier_of(outer_declaration),
                                     outer_kind, variable_init_kind::normal);
        l.visit_variable_declaration(identifier_of(declaration),
                                     variable_kind::_interface,
                                     variable_init_kind::normal);
        l.visit_enter_block_scope();
        visit_kind.visit(l);
        l.visit_exit_block_scope();
        l.visit_end_of_module();

        EXPECT_THAT(v.errors, visit_kind.get_diags_matcher(outer_kind));
      }

      {
        // let I;
        // interface I {}
        // I;
        diag_collector v;
        linter l(&v, &default_globals);
        l.visit_variable_declaration(identifier_of(outer_declaration),
                                     outer_kind, variable_init_kind::normal);
        l.visit_variable_declaration(identifier_of(declaration),
                                     variable_kind::_interface,
                                     variable_init_kind::normal);
        visit_kind.visit(l);
        l.visit_end_of_module();

        EXPECT_THAT(v.errors, visit_kind.get_diags_matcher(outer_kind));
      }

      {
        // interface I {}
        // let I;
        // I;
        diag_collector v;
        linter l(&v, &default_globals);
        l.visit_variable_declaration(identifier_of(declaration),
                                     variable_kind::_interface,
                                     variable_init_kind::normal);
        l.visit_variable_declaration(identifier_of(outer_declaration),
                                     outer_kind, variable_init_kind::normal);
        visit_kind.visit(l);
        l.visit_end_of_module();

        EXPECT_THAT(v.errors, visit_kind.get_diags_matcher(outer_kind));
      }

      {
        // (() => {
        //   I;
        // });
        // interface I {}
        // let I;
        diag_collector v;
        linter l(&v, &default_globals);
        l.visit_enter_function_scope();
        l.visit_enter_function_scope_body();
        visit_kind.visit(l);
        l.visit_exit_function_scope();
        l.visit_variable_declaration(identifier_of(declaration),
                                     variable_kind::_interface,
                                     variable_init_kind::normal);
        l.visit_variable_declaration(identifier_of(outer_declaration),
                                     outer_kind, variable_init_kind::normal);
        l.visit_end_of_module();

        EXPECT_THAT(v.errors, visit_kind.get_diags_matcher(outer_kind));
      }
    }
  }
}

TEST(test_lint_type, mixing_non_type_and_type_only_is_okay) {
  const char8 type_declaration[] = u8"C";
  const char8 non_type_declaration[] = u8"C";

  for (variable_kind type_declaration_kind : {variable_kind::_interface}) {
    for (variable_kind non_type_declaration_kind : {
             variable_kind::_catch,
             variable_kind::_const,
             variable_kind::_function,
             variable_kind::_let,
             variable_kind::_parameter,
             variable_kind::_var,
         }) {
      SCOPED_TRACE(type_declaration_kind);
      SCOPED_TRACE(non_type_declaration_kind);

      {
        // interface C {}
        // let C;
        diag_collector v;
        linter l(&v, &default_globals);
        l.visit_variable_declaration(identifier_of(type_declaration),
                                     type_declaration_kind,
                                     variable_init_kind::normal);
        l.visit_variable_declaration(identifier_of(non_type_declaration),
                                     non_type_declaration_kind,
                                     variable_init_kind::normal);
        l.visit_end_of_module();

        EXPECT_THAT(v.errors, IsEmpty());
      }

      {
        // let C;
        // interface C {}
        diag_collector v;
        linter l(&v, &default_globals);
        l.visit_variable_declaration(identifier_of(non_type_declaration),
                                     non_type_declaration_kind,
                                     variable_init_kind::normal);
        l.visit_variable_declaration(identifier_of(type_declaration),
                                     type_declaration_kind,
                                     variable_init_kind::normal);
        l.visit_end_of_module();

        EXPECT_THAT(v.errors, IsEmpty());
      }
    }
  }
}

TEST(test_lint_type, interfaces_merge_with_interfaces_and_classes) {
  const char8 interface_declaration[] = u8"C";
  const char8 other_declaration[] = u8"C";

  for (variable_kind other_declaration_kind : {
           variable_kind::_class,
           variable_kind::_interface,
       }) {
    SCOPED_TRACE(other_declaration_kind);

    {
      // interface C {}
      // class C {}
      diag_collector v;
      linter l(&v, &default_globals);
      l.visit_variable_declaration(identifier_of(interface_declaration),
                                   variable_kind::_interface,
                                   variable_init_kind::normal);
      l.visit_variable_declaration(identifier_of(other_declaration),
                                   other_declaration_kind,
                                   variable_init_kind::normal);
      l.visit_end_of_module();

      EXPECT_THAT(v.errors, IsEmpty());
    }

    {
      // class C {}
      // interface C {}
      diag_collector v;
      linter l(&v, &default_globals);
      l.visit_variable_declaration(identifier_of(other_declaration),
                                   other_declaration_kind,
                                   variable_init_kind::normal);
      l.visit_variable_declaration(identifier_of(interface_declaration),
                                   variable_kind::_interface,
                                   variable_init_kind::normal);
      l.visit_end_of_module();

      EXPECT_THAT(v.errors, IsEmpty());
    }
  }
}

// When we import, we don't know whether the imported declaration is type-only
// (interface), runtime-only (function or variable), or mixed (class). We take
// the conservative approach and assume that the user wrote correct code (thus
// we report no diagnostic).
TEST(test_lint_type, mixing_interface_and_import_is_not_an_error) {
  const char8 interface_declaration[] = u8"C";
  const char8 imported_declaration[] = u8"C";

  {
    // import {C} from "module";
    // interface C {}
    diag_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(imported_declaration),
                                 variable_kind::_import,
                                 variable_init_kind::normal);
    l.visit_variable_declaration(identifier_of(interface_declaration),
                                 variable_kind::_interface,
                                 variable_init_kind::normal);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // interface C {}
    // import {C} from "module";
    diag_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(interface_declaration),
                                 variable_kind::_interface,
                                 variable_init_kind::normal);
    l.visit_variable_declaration(identifier_of(imported_declaration),
                                 variable_kind::_import,
                                 variable_init_kind::normal);
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
