// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/fe/diagnostic-types.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;

namespace quick_lint_js {
namespace {
TEST(test_parse_typescript_function,
     return_type_annotation_is_disallowed_in_javascript) {
  {
    padded_string code(u8"function f(): C { }"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.variable_uses, ElementsAre(u8"C"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code,
            diag_typescript_type_annotations_not_allowed_in_javascript,  //
            type_colon, strlen(u8"function f()"), u8":")));
  }
}

TEST(test_parse_typescript_function, function_return_type_annotation) {
  {
    spy_visitor v =
        parse_and_visit_typescript_statement(u8"function f(): C { }"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",       // f
                            "visit_enter_function_scope",       // f
                            "visit_variable_type_use",          // C
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope"));      // }
    EXPECT_THAT(v.variable_uses, ElementsAre(u8"C"));
  }
}

TEST(test_parse_typescript_function, arrow_return_type_annotation) {
  {
    spy_visitor v =
        parse_and_visit_typescript_statement(u8"((param): C => {})"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_function_scope",       //
                            "visit_variable_declaration",       // param
                            "visit_variable_type_use",          // C
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope"));      // }
    EXPECT_THAT(v.variable_uses, ElementsAre(u8"C"));
  }

  {
    spy_visitor v = parse_and_visit_typescript_statement(u8"((): C => {})"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_function_scope",       //
                            "visit_variable_type_use",          // C
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope"));      // }
    EXPECT_THAT(v.variable_uses, ElementsAre(u8"C"));
  }

  {
    spy_visitor v =
        parse_and_visit_typescript_statement(u8"(async (param): C => {})"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_function_scope",       //
                            "visit_variable_declaration",       // param
                            "visit_variable_type_use",          // C
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope"));      // }
    EXPECT_THAT(v.variable_uses, ElementsAre(u8"C"));
  }

  {
    spy_visitor v =
        parse_and_visit_typescript_statement(u8"(async (): C => {})"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_function_scope",       //
                            "visit_variable_type_use",          // C
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope"));      // }
    EXPECT_THAT(v.variable_uses, ElementsAre(u8"C"));
  }
}

TEST(test_parse_typescript_function, object_method_return_type_annotation) {
  {
    spy_visitor v =
        parse_and_visit_typescript_statement(u8"({ method(param): C {} })"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_function_scope",       // method
                            "visit_variable_declaration",       // param
                            "visit_variable_type_use",          // C
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope"));      // }
    EXPECT_THAT(v.variable_uses, ElementsAre(u8"C"));
  }
}

TEST(test_parse_typescript_function, class_method_return_type_annotation) {
  {
    spy_visitor v = parse_and_visit_typescript_statement(
        u8"class C { method(param): C {} }"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_class_scope",          // C
                            "visit_enter_class_scope_body",     // {
                            "visit_property_declaration",       // method
                            "visit_enter_function_scope",       // method
                            "visit_variable_declaration",       // param
                            "visit_variable_type_use",          // C
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope",        // }
                            "visit_exit_class_scope",           // }
                            "visit_variable_declaration"));     // C
    EXPECT_THAT(v.variable_uses, ElementsAre(u8"C"));
  }
}

TEST(test_parse_typescript_function, interface_method_return_type_annotation) {
  {
    spy_visitor v = parse_and_visit_typescript_statement(
        u8"interface I { method(param): C; }"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",    // I
                            "visit_enter_interface_scope",   // I
                            "visit_property_declaration",    // method
                            "visit_enter_function_scope",    // method
                            "visit_variable_declaration",    // param
                            "visit_variable_type_use",       // C
                            "visit_exit_function_scope",     // method
                            "visit_exit_interface_scope"));  // }
    EXPECT_THAT(v.variable_uses, ElementsAre(u8"C"));
  }
}

TEST(test_parse_typescript_function,
     non_null_assertion_in_parameter_list_is_an_error) {
  {
    padded_string code(u8"function f(param!) {}"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",       // f
                            "visit_enter_function_scope",       // f
                            "visit_variable_declaration",       // param
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope"));      // }
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code,
                    diag_non_null_assertion_not_allowed_in_parameter,  //
                    bang, strlen(u8"function f(param"), u8"!")));
  }

  {
    padded_string code(u8"(param!) => {}"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_function_scope",       // f
                            "visit_variable_declaration",       // param
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope"));      // }
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code,
                    diag_non_null_assertion_not_allowed_in_parameter,  //
                    bang, strlen(u8"(param"), u8"!")));
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
