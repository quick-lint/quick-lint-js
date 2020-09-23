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

import assert from "assert";
import { loadQuickLintJS } from "./quick-lint-js.mjs";

let tests = {
  "parse and lint returns errors": async () => {
    let input = "undeclared_variable;\nanother_undeclared_variable;\n";
    let qljs = await loadQuickLintJS();
    let marks = qljs.parseAndLint(input);
    assert.deepStrictEqual(marks, [
      { begin: 0, end: "undeclared_variable".length },
      { begin: 21, end: 21 + "another_undeclared_variable".length },
    ]);
  },
};

async function main() {
  for (let testName in tests) {
    if (Object.prototype.hasOwnProperty.call(tests, testName)) {
      let test = tests[testName];
      console.log(`Running ${testName} ...`);
      await test();
    }
  }
  console.log("All tests passed");
}
main().catch((error) => {
  console.error(error.stack);
  process.exit(1);
});
