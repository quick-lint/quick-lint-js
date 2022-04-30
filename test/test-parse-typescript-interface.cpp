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
TEST(test_parse_typescript_interface, not_supported_in_vanilla_javascript) {
  padded_string code(u8"interface I {}"_sv);
  spy_visitor v;
  parser_options options;
  options.typescript = false;
  parser p(&code, &v, options);
  p.parse_and_visit_module(v);
  EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // I
                                    "visit_end_of_module"));
  EXPECT_THAT(v.errors,
              ElementsAre(DIAG_TYPE_OFFSETS(
                  &code,
                  diag_typescript_interfaces_not_allowed_in_javascript,  //
                  interface_keyword, 0, u8"interface")));
}

TEST(test_parse_typescript_interface, empty_interface) {
  padded_string code(u8"interface I {}"_sv);
  spy_visitor v;
  parser p(&code, &v, typescript_options);
  p.parse_and_visit_module(v);
  EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // I
                                    "visit_end_of_module"));
  EXPECT_THAT(
      v.variable_declarations,
      ElementsAre(spy_visitor::visited_variable_declaration{
          u8"I", variable_kind::_interface, variable_init_kind::normal}));
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
