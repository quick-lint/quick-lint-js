// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/identifier-support.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/lint.h>

using ::testing::ElementsAre;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
global_declared_variable_set default_globals = configuration().globals();

TEST(test_lint_enum, member_initializers_can_reference_other_members) {
  const char8 enum_declaration[] = u8"E";
  const char8 member_use[] = u8"A";

  // enum E {
  //   A = 42,
  //   B = A,
  // }
  diag_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(enum_declaration),
                               variable_kind::_enum,
                               variable_init_kind::normal);
  l.visit_enter_enum_scope();
  l.visit_variable_use(identifier_of(member_use));
  l.visit_exit_enum_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
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
