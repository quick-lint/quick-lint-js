// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>

using ::testing::ElementsAreArray;

namespace quick_lint_js {
namespace {
class Test_Parse_Decorator : public ::testing::Test {};

TEST_F(Test_Parse_Decorator, class_statement_decorator) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"@myDecorator\nclass C {}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",            // myDecorator
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myDecorator"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"@myNamespace.myDecorator\nclass C {}"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myNamespace"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"@myDecorator(arg1, arg2)\nclass C {}"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myDecorator"_sv,
                                                   u8"arg1"_sv, u8"arg2"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"@(2 + [myvar,])\nclass C {}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myvar"_sv}));
  }
}

TEST_F(Test_Parse_Decorator, export_class_decorator) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export @myDecorator class C {}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",            // myDecorator
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myDecorator"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"@myDecorator export class C {}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",            // myDecorator
                              "visit_enter_class_scope",       // class
                              "visit_enter_class_scope_body",  // {
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myDecorator"_sv}));
  }
}

TEST_F(Test_Parse_Decorator, export_default_class_decorator) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export default @myDecorator class C {}"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",            // myDecorator
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myDecorator"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export default @myDecorator class {}"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",            // myDecorator
                              "visit_enter_class_scope",       // class
                              "visit_enter_class_scope_body",  // {
                              "visit_exit_class_scope",        // }
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myDecorator"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"@myDecorator export default class {}"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",            // myDecorator
                              "visit_enter_class_scope",       // class
                              "visit_enter_class_scope_body",  // {
                              "visit_exit_class_scope",        // }
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myDecorator"_sv}));
  }
}

TEST_F(Test_Parse_Decorator, class_expression_decorator) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(@myDecorator class C {});"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",            // myDecorator
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_exit_class_scope",        // }
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myDecorator"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(@myDecorator class {});"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",            // myDecorator
                              "visit_enter_class_scope",       // class
                              "visit_enter_class_scope_body",  // {
                              "visit_exit_class_scope",        // }
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myDecorator"_sv}));
  }
}

TEST_F(Test_Parse_Decorator,
       private_identifiers_are_allowed_as_property_names) {
  Spy_Visitor p = test_parse_and_visit_statement(
      u8"class Outer {\n"_sv
      u8"  static #decorator;\n"_sv
      u8"  method() {\n"_sv
      u8"    @Outer.#decorator\n"_sv
      u8"    class Inner {}\n"_sv
      u8"  }\n"_sv
      u8"}"_sv,
      no_diags, javascript_options);
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Outer"_sv}));
}

TEST_F(Test_Parse_Decorator, keywords_are_allowed_as_property_names) {
  for (String8_View keyword : keywords) {
    test_parse_and_visit_statement(
        concat(u8"@Outer."_sv, keyword, u8" class C {}"_sv), no_diags,
        javascript_options);
  }
}

TEST_F(Test_Parse_Decorator,
       contextual_keywords_are_allowed_as_decorator_name) {
  // TODO(strager): Only allow 'await' in non-async functions.
  // TODO(strager): Only allow 'yield' in non-generator functions.
  for (String8_View keyword :
       contextual_keywords | Dirty_Set<String8>{u8"await", u8"yield"}) {
    SCOPED_TRACE(out_string8(keyword));
    Spy_Visitor p = test_parse_and_visit_statement(
        concat(u8"@"_sv, keyword, u8" class C {}"_sv), no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({keyword}));
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
