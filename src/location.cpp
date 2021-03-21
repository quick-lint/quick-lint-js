// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/char8.h>
#include <quick-lint-js/location.h>

namespace quick_lint_js {
bool operator==(source_code_span x, string8_view y) noexcept {
  return x.string_view() == y;
}

bool operator!=(source_code_span x, string8_view y) noexcept {
  return !(x == y);
}

bool operator==(source_code_span x, source_code_span y) noexcept {
  return x.begin() == y.begin() && x.end() == y.end();
}

bool operator!=(source_code_span x, source_code_span y) noexcept {
  return !(x == y);
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
