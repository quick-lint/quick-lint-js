// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import assert from "assert";

export function html(strings, ...expressions) {
  assert.strictEqual(strings.length, expressions.length + 1);
  let out = [];
  for (let i = 0; i < expressions.length; ++i) {
    out.push(strings[i]);
    if (Array.isArray(expressions[i])) {
      out.push(...expressions[i]);
    } else {
      out.push(expressions[i]);
    }
  }
  out.push(strings[expressions.length]);
  return out.join("");
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
