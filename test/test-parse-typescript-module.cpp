// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diagnostic-types.h>
#include <quick-lint-js/dirty-set.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
TEST(test_parse_typescript_module, type_only_import) {
  {
    spy_visitor v =
        parse_and_visit_typescript_module(u8"import type { T } from 'mod';"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(v.variable_declarations, ElementsAre(import_type_decl(u8"T")));
  }

  {
    spy_visitor v = parse_and_visit_typescript_module(
        u8"import type {T as U} from 'mod';"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // U
                                      "visit_end_of_module"));
    EXPECT_THAT(v.variable_declarations, ElementsAre(import_type_decl(u8"U")));
  }

  {
    spy_visitor v =
        parse_and_visit_typescript_module(u8"import type T from 'mod';"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(v.variable_declarations, ElementsAre(import_type_decl(u8"T")));
  }

  {
    spy_visitor v = parse_and_visit_typescript_module(
        u8"import type * as M from 'mod';"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // M
                                      "visit_end_of_module"));
    // TODO(#788): Assert import_module_decl instead.
    EXPECT_THAT(v.variable_declarations, ElementsAre(import_decl(u8"M")));
  }
}

TEST(test_parse, type_only_import_can_declare_contextual_keywords) {
  // TODO(strager): This probably allows more contextual keywords than
  // TypeScript allows.
  for (string8 name :
       contextual_keywords - dirty_set<string8>{u8"from", u8"let"}) {
    {
      padded_string code(u8"import type " + name + u8" from 'mod';");
      SCOPED_TRACE(code);
      spy_visitor v = parse_and_visit_typescript_module(code.string_view());
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // (name)
                                        "visit_end_of_module"));
      EXPECT_THAT(v.variable_declarations, ElementsAre(import_type_decl(name)));
    }
  }
}

TEST(test_parse_typescript_module,
     type_only_import_is_not_allowed_in_javascript) {
  {
    padded_string code(u8"import type {T} from 'mod';"_sv);
    spy_visitor v;
    parser p(&code, &v, javascript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code, diag_typescript_type_only_import_not_allowed_in_javascript,
            type_keyword, strlen(u8"import "), u8"type")));
  }
}

TEST(test_parse_typescript_module,
     type_only_import_cannot_import_default_and_named) {
  {
    padded_string code(u8"import type A, {B} from 'mod';"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // A
                                      "visit_variable_declaration",  // B
                                      "visit_end_of_module"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(import_type_decl(u8"A"), import_decl(u8"B")))
        << "B should be imported as an 'import' not an 'import_type' in case "
           "the user thought that the 'type' keyword only applied to 'A'";
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code,
            diag_typescript_type_only_import_cannot_import_default_and_named,
            type_keyword, strlen(u8"import "), u8"type")));
  }

  {
    padded_string code(u8"import type A, * as B from 'mod';"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // A
                                      "visit_variable_declaration",  // B
                                      "visit_end_of_module"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(import_type_decl(u8"A"), import_decl(u8"B")));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code,
            diag_typescript_type_only_import_cannot_import_default_and_named,
            type_keyword, strlen(u8"import "), u8"type")));
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
