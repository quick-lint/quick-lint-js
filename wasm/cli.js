// Copyright (C) 2020  Matthew "strager" Glazar
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

  let parserForVSCode = await qljsProcess.createParserForVSCodeAsync();
  try {
    parserForVSCode.replaceText(
      {
        start: { line: 0, character: 0 },
        end: { line: 0, character: 0 },
      },
      fileContent
    );
    let diagnostics = parserForVSCode.lint();

    for (let diag of diagnostics) {
      console.log(
        `vscode: ${diag.startLine}:${diag.startCharacter}-${diag.endLine}:${
          diag.endCharacter
        }: ${severityString(diag.severity)}: ${diag.message}`
      );
    }
  } finally {
    parserForVSCode.dispose();
  }

  let parserForWebDemo = await qljsProcess.createParserForWebDemoAsync();
  try {
    parserForWebDemo.setText(fileContent);
    let diagnostics = parserForWebDemo.lint();

    for (let diag of diagnostics) {
      console.log(
        `web demo: ${diag.beginOffset}-${diag.endOffset}: ${severityString(
          diag.severity
        )}: ${diag.message}`
      );
    }
  } finally {
    parserForWebDemo.dispose();
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
