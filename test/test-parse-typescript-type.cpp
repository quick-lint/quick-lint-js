// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <iterator>
#include <quick-lint-js/array.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diagnostic-types.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/spy-visitor.h>
#include <quick-lint-js/string-view.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
TEST(test_parse_typescript_type, direct_type_reference) {
  {
    spy_visitor v = parse_and_visit_typescript_type(u8"Type"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_type_use"));  // Type
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"Type"}));
  }
}

TEST(test_parse_typescript_type, namespaced_type_reference) {
  {
    spy_visitor v = parse_and_visit_typescript_type(u8"ns.Type"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_namespace_use"));  // ns
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"ns"}));
  }
}

TEST(test_parse_typescript_type, builtin_types) {
  for (string8 type : {
           u8"bigint",
           u8"boolean",
           u8"null",
           u8"number",
           u8"object",
           u8"string",
           u8"symbol",
           u8"undefined",
           u8"void",
       }) {
    SCOPED_TRACE(out_string8(type));
    spy_visitor v = parse_and_visit_typescript_type(type);
    EXPECT_THAT(v.visits, IsEmpty());
    EXPECT_THAT(v.variable_uses, IsEmpty())
        << "builtin type should not be treated as a variable";
  }
}

TEST(test_parse_typescript_type, special_types) {
  for (string8 type : {
           u8"any",
           u8"never",
           u8"unknown",
       }) {
    SCOPED_TRACE(out_string8(type));
    spy_visitor v = parse_and_visit_typescript_type(type);
    EXPECT_THAT(v.visits, IsEmpty());
    EXPECT_THAT(v.variable_uses, IsEmpty())
        << "special type should not be treated as a variable";
  }
}

TEST(test_parse_typescript_type, this_type) {
  {
    spy_visitor v = parse_and_visit_typescript_type(u8"this"_sv);
    // TODO(strager): Report an error if the 'this' type is used where 'this'
    // isn't allowed. Should we visit so the linter can report errors? Or should
    // the parser keep track of whether 'this' is permitted?
    EXPECT_THAT(v.visits, IsEmpty());
    EXPECT_THAT(v.variable_uses, IsEmpty());
  }
}

TEST(test_parse_typescript_type, literal_type) {
  {
    spy_visitor v = parse_and_visit_typescript_type(u8"42"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
    EXPECT_THAT(v.variable_uses, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_typescript_type(u8"'hello'"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
    EXPECT_THAT(v.variable_uses, IsEmpty());
  }
}

TEST(test_parse_typescript_type, template_literal_type) {
  {
    spy_visitor v = parse_and_visit_typescript_type(u8"`hello`"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_typescript_type(u8"`hello${other}`"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_type_use"));  // other
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"other"}));
  }

  {
    spy_visitor v =
        parse_and_visit_typescript_type(u8"`hello${other}${another}`"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_type_use",    // other
                                      "visit_variable_type_use"));  // another
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"other"},
                            spy_visitor::visited_variable_use{u8"another"}));
  }
}

TEST(test_parse_typescript_type, parenthesized_type) {
  {
    spy_visitor v = parse_and_visit_typescript_type(u8"(Type)"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_type_use"));  // Type
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"Type"}));
  }

  {
    spy_visitor v = parse_and_visit_typescript_type(u8"(((((Type)))))"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_type_use"));  // Type
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"Type"}));
  }
}

TEST(test_parse_typescript_type, tuple_type) {
  {
    spy_visitor v = parse_and_visit_typescript_type(u8"[]"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
    EXPECT_THAT(v.variable_uses, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_typescript_type(u8"[A]"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_type_use"));  // A
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"A"}));
  }

  {
    spy_visitor v = parse_and_visit_typescript_type(u8"[A, B, C]"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"A"},
                            spy_visitor::visited_variable_use{u8"B"},
                            spy_visitor::visited_variable_use{u8"C"}));
  }

  {
    spy_visitor v = parse_and_visit_typescript_type(u8"[A, B, C, ]"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"A"},
                            spy_visitor::visited_variable_use{u8"B"},
                            spy_visitor::visited_variable_use{u8"C"}));
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
