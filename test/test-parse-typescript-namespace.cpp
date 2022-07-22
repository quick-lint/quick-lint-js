// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/dirty-set.h>
#include <quick-lint-js/fe/diagnostic-types.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
TEST(test_parse_typescript_namespace, not_supported_in_vanilla_javascript) {
  padded_string code(u8"namespace ns {}"_sv);
  spy_visitor v;
  parser p(&code, &v, javascript_options);
  EXPECT_TRUE(p.parse_and_visit_statement(v));
  EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",    // ns
                                    "visit_enter_namespace_scope",   // {
                                    "visit_exit_namespace_scope"));  // }
  EXPECT_THAT(v.errors,
              ElementsAre(DIAG_TYPE_OFFSETS(
                  &code,
                  diag_typescript_namespaces_not_allowed_in_javascript,  //
                  namespace_keyword, 0, u8"namespace")));
}

TEST(test_parse_typescript_namespace, empty_namespace) {
  spy_visitor v = parse_and_visit_typescript_statement(u8"namespace ns {}"_sv);
  EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",    // ns
                                    "visit_enter_namespace_scope",   // {
                                    "visit_exit_namespace_scope"));  // }
  EXPECT_THAT(v.variable_declarations, ElementsAre(namespace_decl(u8"ns")));
}

TEST(test_parse_typescript_namespace,
     namespace_cannot_have_newline_after_namespace_keyword) {
  {
    spy_visitor v = parse_and_visit_typescript_module(u8"namespace\nns\n{}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // namespace
                                      "visit_variable_use",       // ns
                                      "visit_enter_block_scope",  // {
                                      "visit_exit_block_scope",   // }
                                      "visit_end_of_module"));
    EXPECT_THAT(v.variable_uses, ElementsAre(u8"namespace", u8"ns"));
  }
}

TEST(test_parse_typescript_namespace, namespace_can_contain_exports) {
  {
    spy_visitor v = parse_and_visit_typescript_module(
        u8"namespace ns { export function f() {} }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // ns
                                      "visit_enter_namespace_scope",      // {
                                      "visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       // f
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope",        // }
                                      "visit_exit_namespace_scope",       // }
                                      "visit_end_of_module"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(namespace_decl(u8"ns"), function_decl(u8"f")));
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
