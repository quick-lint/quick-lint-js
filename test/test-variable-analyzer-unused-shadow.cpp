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
TEST(test_variable_analyzer_unused_shadow,
     shadowing_initialized_var_without_use_in_block_scope_is_warning) {
  const char8 outer_declaration[] = u8"x";
  const char8 inner_declaration[] = u8"x";

  struct test_case {
    variable_kind outer_declaration_kind;
    variable_kind inner_declaration_kind;
  };
  for (test_case tc : {
           test_case{variable_kind::_const, variable_kind::_const},
           test_case{variable_kind::_const, variable_kind::_let},
           test_case{variable_kind::_let, variable_kind::_const},
           test_case{variable_kind::_let, variable_kind::_let},
           test_case{variable_kind::_var, variable_kind::_const},
           test_case{variable_kind::_var, variable_kind::_let},
       }) {
    SCOPED_TRACE(tc.outer_declaration_kind);
    SCOPED_TRACE(tc.inner_declaration_kind);

    {
      // // const/let/etc.
      // let x = 5;
      // {
      //   // const/let/etc.
      //   let x = 6;  // WARNING
      // }
      diag_collector v;
      variable_analyzer l(&v, &default_globals);
      l.visit_variable_declaration(identifier_of(outer_declaration),
                                   tc.outer_declaration_kind,
                                   variable_init_kind::initialized_with_equals);
      l.visit_enter_block_scope();
      l.visit_variable_declaration(identifier_of(inner_declaration),
                                   tc.inner_declaration_kind,
                                   variable_init_kind::initialized_with_equals);
      l.visit_exit_block_scope();
      l.visit_end_of_module();

      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_2_SPANS(
                      diag_unused_variable_shadows,                       //
                      shadowing_declaration, span_of(inner_declaration),  //
                      shadowed_declaration, span_of(outer_declaration))));
    }

    // TODO(strager): See NOTE[unused-var-shadows-nested-block].
    if ((false)) {
      // // const/let/etc.
      // let x = 5;
      // {
      //   {
      //     // const/let/etc.
      //     let x = 6;  // WARNING
      //   }
      // }
      diag_collector v;
      variable_analyzer l(&v, &default_globals);
      l.visit_variable_declaration(identifier_of(outer_declaration),
                                   tc.outer_declaration_kind,
                                   variable_init_kind::initialized_with_equals);
      l.visit_enter_block_scope();
      l.visit_enter_block_scope();
      l.visit_variable_declaration(identifier_of(inner_declaration),
                                   tc.inner_declaration_kind,
                                   variable_init_kind::initialized_with_equals);
      l.visit_exit_block_scope();
      l.visit_exit_block_scope();
      l.visit_end_of_module();

      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_2_SPANS(
                      diag_unused_variable_shadows,                       //
                      shadowing_declaration, span_of(inner_declaration),  //
                      shadowed_declaration, span_of(outer_declaration))));
    }
  }
}

TEST(test_variable_analyzer_unused_shadow,
     shadowing_function_scope_var_without_use_in_block_scope_is_not_a_warning) {
  const char8 outer_declaration[] = u8"x";
  const char8 inner_declaration[] = u8"x";

  {
    // var x = 5;
    // {
    //   var x = 6;  // no warning
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(outer_declaration),
                                 variable_kind::_var,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(inner_declaration),
                                 variable_kind::_var,
                                 variable_init_kind::initialized_with_equals);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_unused_shadow,
     shadowing_unassigned_var_in_block_scope_is_not_a_warning) {
  const char8 outer_declaration[] = u8"x";
  const char8 inner_declaration[] = u8"x";

  {
    // let x = 5;
    // {
    //   let x;  // no warning
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(outer_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(inner_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::normal);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_unused_shadow,
     shadowing_var_without_use_in_function_scope_is_not_a_warning) {
  const char8 outer_declaration[] = u8"x";
  const char8 inner_declaration[] = u8"x";

  {
    // let x = 5;
    // (function() {
    //   let x = 6;  // no warning
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(outer_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(inner_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_unused_shadow,
     shadowing_parameter_is_not_a_warning) {
  const char8 parameter[] = u8"x";
  const char8 let[] = u8"x";

  // (function(x) {
  //   {
  //     let x = 6;  // no warning
  //   }
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_variable_declaration(identifier_of(parameter),
                               variable_kind::_function_parameter,
                               variable_init_kind::normal);
  l.visit_enter_function_scope_body();
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(let), variable_kind::_let,
                               variable_init_kind::initialized_with_equals);
  l.visit_exit_block_scope();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer_unused_shadow,
     shadowing_class_or_function_or_import_is_not_a_warning) {
  const char8 outer_declaration[] = u8"C";
  const char8 inner_declaration[] = u8"C";

  for (variable_kind outer_kind :
       {variable_kind::_class, variable_kind::_function,
        variable_kind::_import}) {
    SCOPED_TRACE(outer_kind);

    // class C {}
    // // or
    // function C() {}
    // // or
    // import {C} from "module";
    //
    // {
    //   let C = 6;  // no warning
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(outer_declaration), outer_kind,
                                 variable_init_kind::normal);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(inner_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_unused_shadow,
     shadowing_catch_variable_is_not_a_warning) {
  const char8 outer_declaration[] = u8"e";
  const char8 inner_declaration[] = u8"e";

  // try {
  // } catch (e) {
  //   {
  //     let e = 6;  // no warning
  //   }
  // }
  diag_collector v;
  variable_analyzer l(&v, &default_globals);
  l.visit_enter_block_scope();  // try
  l.visit_exit_block_scope();
  l.visit_enter_block_scope();  // catch
  l.visit_variable_declaration(identifier_of(outer_declaration),
                               variable_kind::_catch,
                               variable_init_kind::normal);
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(inner_declaration),
                               variable_kind::_let,
                               variable_init_kind::initialized_with_equals);
  l.visit_exit_block_scope();
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer_unused_shadow,
     using_shadowing_variable_is_not_a_warning) {
  const char8 outer_declaration[] = u8"x";
  const char8 inner_declaration[] = u8"x";
  const char8 use[] = u8"x";

  {
    // let x = 5;
    // {
    //   let x = 6;  // no warning
    //   x;
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(outer_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(inner_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // let x = 5;
    // {
    //   let x = 6;  // no warning
    //   {
    //     x;
    //   }
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(outer_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(inner_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_block_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_block_scope();
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // let x = 5;
    // {
    //   let x = 6;    // no warning
    //   (function() {
    //     x;
    //   });
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(outer_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(inner_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_unused_shadow,
     using_shadowing_variable_before_its_declaration_is_not_a_warning) {
  const char8 outer_declaration[] = u8"x";
  const char8 inner_declaration[] = u8"x";
  const char8 use[] = u8"x";

  {
    // let x = 5;
    // {
    //   x;          // ERROR
    //   let x = 6;  // no warning
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(outer_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_block_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_declaration(identifier_of(inner_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE(diag_variable_used_before_declaration)));
  }

  {
    // let x = 5;
    // {
    //   {
    //     x;        // ERROR
    //   }
    //   let x = 6;  // no warning
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(outer_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_block_scope();
    l.visit_enter_block_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_block_scope();
    l.visit_variable_declaration(identifier_of(inner_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE(diag_variable_used_before_declaration)));
  }

  {
    // let x = 5;
    // {
    //   (function() {
    //     x;          // no error
    //   });
    //   let x = 6;    // no warning
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(outer_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_block_scope();
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_variable_declaration(identifier_of(inner_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_unused_shadow,
     using_shadowing_variable_with_eval_is_not_a_warning) {
  const char8 outer_declaration[] = u8"x";
  const char8 inner_declaration[] = u8"x";
  const char8 use_eval[] = u8"eval";

  {
    // let x = 5;
    // {
    //   let x = 6;  // no warning
    //   eval("x");
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(outer_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(inner_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // let x = 5;
    // {
    //   let x = 6;  // no warning
    //   {
    //     eval("x");
    //   }
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(outer_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(inner_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_block_scope();
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_exit_block_scope();
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // let x = 5;
    // {
    //   let x = 6;  // no warning
    //   {
    //     {
    //       eval("x");
    //     }
    //   }
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(outer_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(inner_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_block_scope();
    l.visit_enter_block_scope();
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_exit_block_scope();
    l.visit_exit_block_scope();
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // let x = 5;
    // {
    //   let x = 6;  // no warning
    //   (function() {
    //     eval("x");
    //   });
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(outer_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(inner_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_exit_function_scope();
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // let x = 5;
    // {
    //   let x = 6;  // no warning
    //   (function() {
    //     (function() {
    //       eval("x");
    //     });
    //   });
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(outer_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(inner_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_exit_function_scope();
    l.visit_exit_function_scope();
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_unused_shadow,
     assigning_to_shadowing_variable_is_not_a_warning) {
  const char8 outer_declaration[] = u8"x";
  const char8 inner_declaration[] = u8"x";
  const char8 assignment[] = u8"x";

  {
    // let x = 5;
    // {
    //   let x = 6;  // no warning
    //   x = 7;
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(outer_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(inner_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_variable_assignment(identifier_of(assignment));
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
