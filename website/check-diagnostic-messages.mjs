// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import path from "path";
import {
  ErrorDocumentation,
  ProblemsError,
  documentationDirectoryPath,
  loadErrorDocumentationFilesAsync,
} from "./src/error-documentation.mjs";

async function mainAsync() {
  let files = process.argv.slice(2);
  let documents;
  if (files.length === 0) {
    documents = await loadErrorDocumentationFilesAsync(
      documentationDirectoryPath
    );
  } else {
    documents = await Promise.all(
      files.map(
        async (fileName) => await ErrorDocumentation.parseFileAsync(fileName)
      )
    );
  }

  let locales = ["", "de", "en@loud", "en_US@snarky", "fr_FR", "sv_SE"];
  let maxLocaleLength = Math.max(...locales.map((l) => l.length));

  let cwd = process.cwd();
  for (let doc of documents) {
    let diagnosticsByLocale = {};
    for (let locale of locales) {
      await doc.findDiagnosticsAsync(locale);
      diagnosticsByLocale[locale] = doc.diagnostics;
    }

    for (
      let snippetIndex = 0;
      snippetIndex < diagnosticsByLocale[locales[0]].length;
      ++snippetIndex
    ) {
      console.log(
        `${path.relative(cwd, doc.filePath)} snippet ${snippetIndex + 1}:`
      );
      for (
        let diagIndex = 0;
        diagIndex < diagnosticsByLocale[locales[0]][snippetIndex].length;
        ++diagIndex
      ) {
        for (let locale of locales) {
          let diag = diagnosticsByLocale[locale][snippetIndex][diagIndex];
          let begin = diag.begin.toString().padStart(3);
          let end = diag.end.toString().padStart(3);
          console.log(
            `  [${locale.padEnd(maxLocaleLength)}] ${begin}-${end}: ${
              diag.severity
            }(${diag.code}): ${diag.message}`
          );
        }
      }
      if (doc.diagnostics[snippetIndex].length === 0) {
        console.log("  (none)");
      }
    }
  }
}

mainAsync().catch((error) => {
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
