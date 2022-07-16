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

TEST(test_parse_typescript_module, inline_type_import) {
  {
    spy_visitor v =
        parse_and_visit_typescript_module(u8"import {type T} from 'mod';"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(v.variable_declarations, ElementsAre(import_type_decl(u8"T")));
  }

  {
    spy_visitor v = parse_and_visit_typescript_module(
        u8"import {type T, type U} from 'mod';"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // T
                                      "visit_variable_declaration",  // U
                                      "visit_end_of_module"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(import_type_decl(u8"T"), import_type_decl(u8"U")));
  }

  {
    spy_visitor v = parse_and_visit_typescript_module(
        u8"import {type T as U} from 'mod';"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // U
                                      "visit_end_of_module"));
    EXPECT_THAT(v.variable_declarations, ElementsAre(import_type_decl(u8"U")));
  }
}

TEST(test_parse_typescript_module, mixed_inline_type_and_normal_import) {
  {
    spy_visitor v = parse_and_visit_typescript_module(
        u8"import {type T, f} from 'mod';"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // T
                                      "visit_variable_declaration",  // f
                                      "visit_end_of_module"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(import_type_decl(u8"T"), import_decl(u8"f")));
  }

  {
    spy_visitor v = parse_and_visit_typescript_module(
        u8"import {f, type T} from 'mod';"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // f
                                      "visit_variable_declaration",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(import_decl(u8"f"), import_type_decl(u8"T")));
  }
}

TEST(test_parse, inline_type_import_can_declare_contextual_keywords) {
  // TODO(strager): This probably allows more contextual keywords than
  // TypeScript allows.
  for (string8 name : contextual_keywords - dirty_set<string8>{u8"let"}) {
    {
      padded_string code(u8"import {type " + name + u8"} from 'mod';");
      SCOPED_TRACE(code);
      spy_visitor v = parse_and_visit_typescript_module(code.string_view());
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // (name)
                                        "visit_end_of_module"));
      EXPECT_THAT(v.variable_declarations, ElementsAre(import_type_decl(name)));
    }

    {
      padded_string code(u8"import {type " + name + u8", other} from 'mod';");
      SCOPED_TRACE(code);
      spy_visitor v = parse_and_visit_typescript_module(code.string_view());
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // (name)
                                        "visit_variable_declaration",  // other
                                        "visit_end_of_module"));
      EXPECT_THAT(v.variable_declarations,
                  ElementsAre(import_type_decl(name), import_decl(u8"other")));
    }
  }
}

TEST(test_parse_typescript_module,
     inline_type_import_is_not_allowed_in_javascript) {
  {
    padded_string code(u8"import {type T} from 'mod';"_sv);
    spy_visitor v;
    parser p(&code, &v, javascript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code, diag_typescript_inline_type_import_not_allowed_in_javascript,
            type_keyword, strlen(u8"import {"), u8"type")));
  }

  {
    padded_string code(u8"import {type as} from 'mod';"_sv);
    spy_visitor v;
    parser p(&code, &v, javascript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // as
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code, diag_typescript_inline_type_import_not_allowed_in_javascript,
            type_keyword, strlen(u8"import {"), u8"type")));
  }
}

TEST(test_parse_typescript_module, mixed_inline_type_and_type_only_import) {
  {
    padded_string code(u8"import type {type T} from 'mod';"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(v.variable_declarations, ElementsAre(import_type_decl(u8"T")));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            &code,
            diag_typescript_inline_type_import_not_allowed_in_type_only_import,
            inline_type_keyword, strlen(u8"import type {"), u8"type",  //
            type_only_keyword, strlen(u8"import "), u8"type")));
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
