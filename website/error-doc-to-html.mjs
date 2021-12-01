// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import { ErrorDocumentation } from "./src/error-documentation.mjs";
import process from "process";

async function mainAsync() {
  let docFilePath = process.argv[2];

  let doc = await ErrorDocumentation.parseFileAsync(docFilePath);
  let problems = await doc.findProblemsAsync();
  if (problems.length !== 0) {
    console.error(`${problems.length} problems found:`);
    for (let problem of problems) {
      console.error(problem);
    }
    process.exit(1);
  }
  console.log(doc.toHTML());
}

mainAsync().catch((e) => {
  console.error(e.stack);
  process.exit(1);
});

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
