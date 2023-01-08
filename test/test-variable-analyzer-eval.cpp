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
TEST(test_variable_analyzer_eval_javascript,
     disable_variable_lookup_in_presence_of_eval) {
  const char8 use_eval[] = u8"eval";
  const char8 use[] = u8"x";

  {
    // eval("var x = 42");
    // x;
    // x = 10;
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // {
    //   eval("var x = 42");
    // }
    // x;
    // x = 10;
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_block_scope();
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_exit_block_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // eval("var x = 42");
    // (function() {
    //   x;
    //   x = 10;
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // (function() {
    //   x;
    //   x = 10;
    // });
    // eval("var x = 42");
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // (function() {
    //   x;
    //   x = 10;
    //   eval("var x = 42;");
    //   x;
    //   x = 10;
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // (function() {
    //   x;
    //   x = 10;
    //   {
    //     eval("var x = 42;");
    //   }
    //   x;
    //   x = 10;
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_enter_block_scope();
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_exit_block_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_eval_javascript,
     disable_variable_lookup_in_presence_of_eval_for_limited_scope) {
  const char8 use_eval[] = u8"eval";
  const char8 use[] = u8"x";

  {
    // (function() {
    //   eval("var x = 42;");
    // });
    // (function() {
    //   x;  // ERROR (use of undeclared variable)
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_exit_function_scope();
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name, span_of(use)),
        }));
  }

  {
    // (function() {
    //   eval("var x = 42;");
    // });
    // x;  // ERROR (use of undeclared variable)
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_exit_function_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name, span_of(use)),
        }));
  }

  {
    // (function() {
    //   (function() {
    //     eval("var x = 42;");
    //   });
    //   x;      // ERROR (use of undeclared variable)
    //   x = 10; // ERROR (assignment to undeclared variable)
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_exit_function_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name, span_of(use)),
            DIAG_TYPE_SPAN(diag_assignment_to_undeclared_variable, assignment,
                           span_of(use)),
        }));
  }

  {
    const char8 parameter_declaration[] = u8"a";
    const char8 function_declaration[] = u8"f";

    // function f(a = eval('var x = 42;')) {
    //   x;
    // }
    // x; // ERROR (use of undeclared variable)
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_named_function_scope(identifier_of(function_declaration));
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_function_parameter,
                                 variable_init_kind::normal);
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name, span_of(use)),
        }));
  }

  {
    const char8 eval_declaration[] = u8"eval";

    // let eval = () => {};
    // eval("var x = 42;");
    // x;  // ERROR (use of undeclared variable)
    // x = 10;  // ERROR (assignment to undeclared variable)
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(eval_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name, span_of(use)),
            DIAG_TYPE_SPAN(diag_assignment_to_undeclared_variable, assignment,
                           span_of(use)),
        }));
  }

  {
    const char8 eval_declaration[] = u8"eval";

    // function f() {
    //   eval = () => {};
    //   eval("var x = 42;");
    //   x;  // ERROR (use of undeclared variable)
    //   x = 10;  // ERROR (assignment to undeclared variable)
    //   {
    //     var eval;
    //   }
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_assignment(identifier_of(use_eval));
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(eval_declaration),
                                 variable_kind::_var,
                                 variable_init_kind::normal);
    l.visit_exit_block_scope();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name, span_of(use)),
            DIAG_TYPE_SPAN(diag_assignment_to_undeclared_variable, assignment,
                           span_of(use)),
        }));
  }
}

TEST(test_variable_analyzer_eval_javascript,
     false_negatives_on_redeclaration_of_eval) {
  const char8 use_eval[] = u8"eval";
  const char8 use[] = u8"x";

  {
    const char8 eval_declaration[] = u8"eval";

    // let eval = () => {};
    // (function() {
    //   eval("var x = 42;");
    //   x;  // TODO: ERROR (use of undeclared variable)
    //   x = 10;  // TODO: ERROR (assignment to undeclared variable)
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(eval_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    const char8 const_declaration[] = u8"x";
    const char8 const_assignment[] = u8"x";

    // (function() {
    //   const x = 42;
    //   {
    //     eval("var x = 0");
    //     x = 3;  // TODO: ERROR (assignment to const variable)
    //   }
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(const_declaration),
                                 variable_kind::_const,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_block_scope();
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_variable_assignment(identifier_of(const_assignment));
    l.visit_exit_block_scope();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_eval_typescript,
     eval_does_not_disable_variable_lookup) {
  const char8 use_eval[] = u8"eval";
  const char8 use[] = u8"x";
  const char8 assignment[] = u8"x";

  {
    // eval("var x = 42");
    // x;                   // ERROR
    // x = 10;              // ERROR
    diag_collector v;
    variable_analyzer l(&v, &default_globals, typescript_var_options);
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        UnorderedElementsAre(
            DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name, span_of(use)),
            DIAG_TYPE_SPAN(diag_assignment_to_undeclared_variable, assignment,
                           span_of(assignment))));
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
