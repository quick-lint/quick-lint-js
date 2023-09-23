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

TEST_F(Test_Parse_Decorator,
       decorator_is_not_allowed_both_before_and_after_export) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"@d1 export @d2 class C {}"_sv,  //
        u8"           ^ Diag_Decorator_Before_And_After_Export_Keyword.decorator_at_after\n"_diag
        u8"^ .decorator_at_before"_diag,
        javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"d1"_sv, u8"d2"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"@d1 export default @d2 class {}"_sv,  //
        u8"                   ^ Diag_Decorator_Before_And_After_Export_Keyword.decorator_at_after\n"_diag
        u8"^ .decorator_at_before"_diag,
        javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"d1"_sv, u8"d2"_sv}));
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

TEST_F(Test_Parse_Decorator, multiple_decorators_on_class) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"@decorator1\n"_sv
        u8"@(decorator2)\n"_sv
        u8"@decorator3(x)\n"_sv
        u8"class C {}"_sv,
        no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"decorator1"_sv, u8"decorator2"_sv,
                                  u8"decorator3"_sv, u8"x"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export\n"_sv
        u8"  @decorator1\n"_sv
        u8"  @(decorator2)\n"_sv
        u8"  @decorator3(x)\n"_sv
        u8"  class C {}"_sv,
        no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"decorator1"_sv, u8"decorator2"_sv,
                                  u8"decorator3"_sv, u8"x"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export default\n"_sv
        u8"  @decorator1\n"_sv
        u8"  @(decorator2)\n"_sv
        u8"  @decorator3(x)\n"_sv
        u8"  class {}"_sv,
        no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"decorator1"_sv, u8"decorator2"_sv,
                                  u8"decorator3"_sv, u8"x"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"const C ="_sv
        u8"  @decorator1\n"_sv
        u8"  @(decorator2)\n"_sv
        u8"  @decorator3(x)\n"_sv
        u8"  class {};"_sv,
        no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"decorator1"_sv, u8"decorator2"_sv,
                                  u8"decorator3"_sv, u8"x"_sv}));
  }
}

TEST_F(Test_Parse_Decorator, class_methods_can_have_decorator) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { @decorator method() {} }"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"decorator"_sv}));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { @decorator static method() {} }"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"decorator"_sv}));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { @decorator1 @(decorator2) @decorator3(x) method() {} }"_sv,
        no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"decorator1"_sv, u8"decorator2"_sv,
                                  u8"decorator3"_sv, u8"x"_sv}));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"_sv}));
  }
}

TEST_F(Test_Parse_Decorator, class_fields_can_have_decorator) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { @decorator myField; }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"decorator"_sv}));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"myField"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { @decorator static myField; }"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"decorator"_sv}));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"myField"_sv}));
  }
}

TEST_F(Test_Parse_Decorator, class_accessors_can_have_decorator) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { @decorator accessor myProp; }"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"decorator"_sv}));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"myProp"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { @decorator static accessor myProp; }"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"decorator"_sv}));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"myProp"_sv}));
  }
}

TEST_F(Test_Parse_Decorator, class_static_block_cannot_have_accessor) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { @decorator static { console.log('hi'); } }"_sv,
        u8"                     ^^^^^^ Diag_Decorator_Not_Allowed_On_Class_Static_Block.static_keyword\n"_diag
        u8"          ^ .decorator_at"_diag,
        javascript_options);
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"decorator"_sv, u8"console"_sv}));
  }
}

TEST_F(Test_Parse_Decorator,
       decorator_must_appear_before_class_member_modifiers) {
  test_parse_and_visit_statement(
      u8"class C { static @decorator foo() {} }"_sv,
      u8"                 ^ Diag_Decorator_After_Class_Member_Modifiers.decorator_at\n"_diag
      u8"          ^^^^^^ .modifier"_diag,
      javascript_options);
  test_parse_and_visit_statement(
      u8"class C { @decorator1 static @decorator2 foo() {} }"_sv,
      u8"                             ^ Diag_Decorator_After_Class_Member_Modifiers.decorator_at\n"_diag
      u8"                      ^^^^^^ .modifier"_diag,
      javascript_options);
  test_parse_and_visit_statement(
      u8"class C { public static @decorator1 @decorator2 async foo() {} }"_sv,
      u8"                                    ^ Diag_Decorator_After_Class_Member_Modifiers.decorator_at\n"_diag
      u8"          ^^^^^^ .modifier"_diag,
      u8"                        ^ Diag_Decorator_After_Class_Member_Modifiers.decorator_at\n"_diag
      u8"          ^^^^^^ .modifier"_diag,
      typescript_options);
  test_parse_and_visit_statement(
      u8"class C { static @decorator foo: number = 42; }"_sv,
      u8"                 ^ Diag_Decorator_After_Class_Member_Modifiers.decorator_at\n"_diag
      u8"          ^^^^^^ .modifier"_diag,
      typescript_options);
}

TEST_F(Test_Parse_Decorator, decorator_must_decorate_something_inside_class) {
  test_parse_and_visit_statement(
      u8"class C { @decorator }"_sv,
      u8"                    ` Diag_Missing_Class_Member_After_Decorator.expected_member\n"_diag
      u8"          ^ .decorator_at"_diag,
      javascript_options);
  test_parse_and_visit_statement(
      u8"class C { @decorator ; }"_sv,
      u8"                    ` Diag_Missing_Class_Member_After_Decorator.expected_member\n"_diag
      u8"          ^ .decorator_at"_diag,
      javascript_options);
}

TEST_F(Test_Parse_Decorator, semicolon_is_not_allowed_after_decorator) {
  test_parse_and_visit_statement(
      u8"class C { @decorator; foo() {} }"_sv,
      u8"                    ^ Diag_Unexpected_Semicolon_After_Decorator.semicolon\n"_diag
      u8"          ^ .decorator_at"_diag,
      javascript_options);
}

TEST_F(Test_Parse_Decorator,
       decorators_are_not_allowed_in_typescript_interfaces) {
  test_parse_and_visit_statement(
      u8"interface I { @decorator foo(); }"_sv,
      u8"              ^ Diag_Decorator_In_TypeScript_Interface"_diag,
      typescript_options);

  // Shouldn't also report Diag_Missing_Class_Member_After_Decorator.
  test_parse_and_visit_statement(
      u8"interface I { @decorator }"_sv,
      u8"              ^ Diag_Decorator_In_TypeScript_Interface"_diag,
      typescript_options);
  // Shouldn't also report Diag_Missing_Class_Member_After_Decorator.
  test_parse_and_visit_statement(
      u8"interface I { @decorator; }"_sv,
      u8"              ^ Diag_Decorator_In_TypeScript_Interface"_diag,
      typescript_options);
  // Shouldn't also report Diag_Unexpected_Semicolon_After_Decorator.
  test_parse_and_visit_statement(
      u8"interface I { @decorator; foo(); }"_sv,
      u8"              ^ Diag_Decorator_In_TypeScript_Interface"_diag,
      typescript_options);
  // Shouldn't also report Diag_Decorator_After_Class_Member_Modifiers.
  test_parse_and_visit_statement(
      u8"interface I { readonly @decorator field; }"_sv,
      u8"                       ^ Diag_Decorator_In_TypeScript_Interface"_diag,
      typescript_options);
}

TEST_F(Test_Parse_Decorator,
       decorators_are_not_allowed_on_typescript_abstract_methods) {
  test_parse_and_visit_statement(
      u8"abstract class C { @decorator abstract foo(); }"_sv,
      u8"                              ^^^^^^^^ Diag_Decorator_On_Abstract_Class_Member.abstract_keyword\n"_diag
      u8"                   ^ .decorator_at"_diag,
      typescript_options);
  test_parse_and_visit_statement(
      u8"abstract class C { @decorator abstract myField; }"_sv,
      u8"                              ^^^^^^^^ Diag_Decorator_On_Abstract_Class_Member.abstract_keyword\n"_diag
      u8"                   ^ .decorator_at"_diag,
      typescript_options);
  test_parse_and_visit_statement(
      u8"abstract class C { @decorator1 @decorator2 abstract foo(); }"_sv,
      u8"                                           ^^^^^^^^ Diag_Decorator_On_Abstract_Class_Member.abstract_keyword\n"_diag
      u8"                               ^ .decorator_at"_diag,
      u8"                                           ^^^^^^^^ Diag_Decorator_On_Abstract_Class_Member.abstract_keyword\n"_diag
      u8"                   ^ .decorator_at"_diag,
      typescript_options);
  // Shouldn't also report Diag_Decorator_After_Class_Member_Modifiers.
  test_parse_and_visit_statement(
      u8"abstract class C { abstract @decorator foo(); }"_sv,
      u8"                            ^ Diag_Decorator_On_Abstract_Class_Member.decorator_at\n"_diag
      u8"                   ^^^^^^^^ .abstract_keyword"_diag,
      typescript_options);
}

TEST_F(Test_Parse_Decorator, decorator_on_typescript_overloaded_method) {
  test_parse_and_visit_statement(u8"class C { foo(); @decorator foo() {} }"_sv,
                                 no_diags, typescript_options);
}

TEST_F(Test_Parse_Decorator,
       decorators_are_not_allowed_on_typescript_method_overload_signatures) {
  test_parse_and_visit_statement(
      u8"class C { @decorator foo(); foo() {} }"_sv,
      u8"                            ` Diag_Decorator_On_Overload_Signature.expected_location\n"_diag
      u8"          ^ .decorator_at"_diag,
      typescript_options);
  test_parse_and_visit_statement(
      u8"class C { @decorator foo(); async foo() {} }"_sv,
      u8"                            ` Diag_Decorator_On_Overload_Signature.expected_location\n"_diag
      u8"          ^ .decorator_at"_diag,
      typescript_options);
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
