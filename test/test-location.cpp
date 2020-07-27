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

#include <doctest/doctest.h>
#include <quick-lint-js/location.h>

namespace quick_lint_js {
namespace {
TEST_CASE("ranges on first line") {
  const char code[] = "let x = 2;";
  locator l(code);
  source_range x_range = l.range(source_code_span(&code[4], &code[5]));

  CHECK(x_range.begin_offset() == 4);
  CHECK(x_range.begin().line_number == 1);
  CHECK(x_range.begin().column_number == 5);

  CHECK(x_range.end_offset() == 5);
  CHECK(x_range.end().line_number == 1);
  CHECK(x_range.end().column_number == 6);
}

TEST_CASE("ranges on second line") {
  const char code[] = "let x = 2;\nlet y = 3;";
  locator l(code);
  source_range x_range = l.range(source_code_span(&code[15], &code[16]));

  CHECK(x_range.begin_offset() == 15);
  CHECK(x_range.begin().line_number == 2);
  CHECK(x_range.begin().column_number == 5);

  CHECK(x_range.end_offset() == 16);
  CHECK(x_range.end().line_number == 2);
  CHECK(x_range.end().column_number == 6);
}
}  // namespace
}  // namespace quick_lint_js
