// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/dirty-set.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
class test_parse_typescript_namespace : public test_parse_expression {};

TEST_F(test_parse_typescript_namespace, not_supported_in_vanilla_javascript) {
  test_parser p(u8"namespace ns {}"_sv, javascript_options, capture_diags);
  p.parse_and_visit_statement();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_declaration",   // ns
                            "visit_enter_namespace_scope",  // {
                            "visit_exit_namespace_scope",   // }
                        }));
  EXPECT_THAT(p.errors,
              ElementsAreArray({
                  DIAG_TYPE_OFFSETS(
                      p.code,
                      diag_typescript_namespaces_not_allowed_in_javascript,  //
                      namespace_keyword, 0, u8"namespace"_sv),
              }));
}

TEST_F(test_parse_typescript_namespace, empty_namespace) {
  test_parser p(u8"namespace ns {}"_sv, typescript_options);
  p.parse_and_visit_statement();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_declaration",   // ns
                            "visit_enter_namespace_scope",  // {
                            "visit_exit_namespace_scope",   // }
                        }));
  EXPECT_THAT(p.variable_declarations,
              ElementsAreArray({namespace_decl(u8"ns"_sv)}));
}

TEST_F(test_parse_typescript_namespace, missing_body) {
  {
    test_parser p(u8"namespace ns "_sv, typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // ns
                              "visit_end_of_module",         //
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_missing_body_for_typescript_namespace,  //
                              expected_body, strlen(u8"namespace ns"), u8""_sv),
        }));
  }

  {
    test_parser p(u8"namespace ns\nconsole.log('hello');"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // ns
                              "visit_variable_use",          // console
                              "visit_end_of_module",         //
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_missing_body_for_typescript_namespace,  //
                              expected_body, strlen(u8"namespace ns"), u8""_sv),
        }));
  }
}

TEST_F(test_parse_typescript_namespace,
       namespace_cannot_have_newline_after_namespace_keyword) {
  {
    test_parser p(u8"namespace\nns {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // ns
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({namespace_decl(u8"ns"_sv)}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        diag_newline_not_allowed_after_namespace_keyword,  //
                        namespace_keyword, 0, u8"namespace"_sv),
                }));
  }
}

TEST_F(test_parse_typescript_namespace,
       namespace_keyword_with_following_newline_is_variable_name) {
  {
    test_parser p(u8"namespace\nns\n{}"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // namespace
                              "visit_variable_use",       // ns
                              "visit_enter_block_scope",  // {
                              "visit_exit_block_scope",   // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"namespace", u8"ns"}));
  }
}

TEST_F(test_parse_typescript_namespace,
       namespace_name_can_be_contextual_keyword) {
  for (string8 name :
       contextual_keywords - dirty_set<string8>{u8"let", u8"static"}) {
    padded_string code(concat(u8"namespace "_sv, name, u8" {}"_sv));
    SCOPED_TRACE(code);
    test_parser p(code.string_view(), typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({namespace_decl(name)}));
  }
}

TEST_F(test_parse_typescript_namespace, namespace_can_contain_exports) {
  {
    test_parser p(u8"namespace ns { export function f() {} }"_sv,
                  typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // ns
                              "visit_enter_namespace_scope",      // {
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_exit_namespace_scope",       // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({namespace_decl(u8"ns"_sv), function_decl(u8"f"_sv)}));
  }
}

TEST_F(test_parse_typescript_namespace, namespace_alias) {
  {
    test_parser p(u8"import A = ns;"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // A
                              "visit_variable_namespace_use",  // ns
                          }));
    // TODO(#793): Instead of emitting an import declaration, we should emit a
    // import alias declaration. Use-before-declaration is okay for ES imports,
    // but is problematic for namespace aliases.
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_decl(u8"A"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ns"}));
  }
}

TEST_F(test_parse_typescript_namespace,
       namespace_alias_not_allowed_in_javascript) {
  {
    test_parser p(u8"import A = ns;"_sv, javascript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // A
                              "visit_variable_namespace_use",  // ns
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_typescript_import_alias_not_allowed_in_javascript,  //
                import_keyword, 0, u8"import"_sv, equal, strlen(u8"import A "),
                u8"="_sv),
        }));
  }
}

TEST_F(test_parse_typescript_namespace, import_alias_of_namespace_member) {
  {
    test_parser p(u8"import A = ns.B;"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // A
                              "visit_variable_namespace_use",  // ns
                              "visit_end_of_module",
                          }));
    // TODO(#793): Emit a import alias declaration instead.
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_decl(u8"A"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ns"}));
  }

  {
    test_parser p(u8"import A = ns.subns.B;"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // A
                              "visit_variable_namespace_use",  // ns
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ns"}));
  }
}

TEST_F(test_parse_typescript_namespace,
       import_alias_requires_semicolon_or_newline) {
  {
    test_parser p(u8"import A = ns nextStatement"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // A
                              "visit_variable_namespace_use",  // ns
                              "visit_variable_use",            // nextStatement
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_missing_semicolon_after_statement,  //
                              where, strlen(u8"import A = ns"), u8""_sv),
        }));
  }
}

TEST_F(test_parse_typescript_namespace,
       namespace_can_be_contextual_keyword_in_import_alias) {
  for (string8 name : contextual_keywords) {
    padded_string code(concat(u8"import A = "_sv, name, u8".Member;"_sv));
    SCOPED_TRACE(code);
    test_parser p(code.string_view(), typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // A
                              "visit_variable_namespace_use",  // (name)
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({name}));
  }
}

TEST_F(test_parse_typescript_namespace,
       namespace_member_can_be_contextual_keyword_in_import_alias) {
  for (string8 name : contextual_keywords) {
    padded_string code(concat(u8"import A = ns."_sv, name, u8";"_sv));
    SCOPED_TRACE(code);
    test_parser p(code.string_view(), typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // A
                              "visit_variable_namespace_use",  // ns
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ns"}));
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
