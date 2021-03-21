// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

"use strict";

let {
  DiagnosticSeverity,
  createProcessFactoryAsync,
} = require("./quick-lint-js.js");
let fs = require("fs");

async function main() {
  let fileName = process.argv[2];
  let fileContent = fs.readFileSync(fileName, "utf-8");

  let factory = await createProcessFactoryAsync();
  let qljsProcess = await factory.createProcessAsync();
  let parser = await qljsProcess.createParserAsync();
  try {
    parser.replaceText(
      {
        start: { line: 0, character: 0 },
        end: { line: 0, character: 0 },
      },
      fileContent
    );
    let diagnostics = parser.lint();

    for (let diag of diagnostics) {
      console.log(
        `${diag.startLine}:${diag.startCharacter}-${diag.endLine}:${
          diag.endCharacter
        }: ${severityString(diag.severity)}: ${diag.message}`
      );
    }
  } finally {
    parser.dispose();
  }
}

function severityString(severity) {
  switch (severity) {
    case DiagnosticSeverity.ERROR:
      return "error";
    case DiagnosticSeverity.WARNING:
      return "warning";
    default:
      return `??? (severity ${severity})`;
  }
}

main().catch((error) => {
  console.error(error.stack);
  process.exit(1);
});

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
