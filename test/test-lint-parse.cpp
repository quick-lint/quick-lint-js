// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <cstring>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse.h>

namespace quick_lint_js {
TEST(test_lint, let_variable_use_before_declaration_with_parsing) {
  padded_string input(u8"let x = y, y = x;");
  error_collector v;
  linter l(&v);
  parser p(&input, &v);
  p.parse_and_visit_statement(l);
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 1);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_variable_used_before_declaration);
  EXPECT_EQ(locator(&input).range(v.errors[0].where).begin_offset(), 8);
  EXPECT_EQ(locator(&input).range(v.errors[0].where).end_offset(), 9);
  EXPECT_EQ(locator(&input).range(v.errors[0].other_where).begin_offset(), 11);
}
}
