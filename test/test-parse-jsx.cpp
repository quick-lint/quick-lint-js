// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/error-matcher.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::Contains;
using ::testing::ElementsAre;

namespace quick_lint_js {
namespace {
TEST(test_parse, jsx_is_not_supported) {
  // If parsing was not started with
  // parse_and_visit_module_catching_fatal_parse_errors, then we can't halt
  // parsing at the '<'. Error recovery will do a bad job.
  padded_string code(u8"<MyComponent attr={value}>hello</MyComponent>"_sv);
  spy_visitor v;
  parser p(&code, &v);
  p.parse_and_visit_module(v);
  EXPECT_THAT(v.errors, Contains(ERROR_TYPE_FIELD(
                            error_jsx_not_yet_implemented, jsx_start,
                            offsets_matcher(&code, 0, u8"<"))));
}

#if QLJS_HAVE_SETJMP
TEST(test_parse, parsing_stops_on_jsx) {
  padded_string code(u8"<MyComponent attr={value}>hello</MyComponent>"_sv);
  spy_visitor v;
  parser p(&code, &v);
  bool ok = p.parse_and_visit_module_catching_fatal_parse_errors(v);
  EXPECT_FALSE(ok);
  EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                            error_jsx_not_yet_implemented, jsx_start,
                            offsets_matcher(&code, 0, u8"<"))));
}
#endif
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
