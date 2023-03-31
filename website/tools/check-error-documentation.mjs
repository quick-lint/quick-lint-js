// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import {
  ErrorDocumentation,
  ProblemsError,
  documentationDirectoryPath,
  loadErrorDocumentationFilesAsync,
} from "../src/error-documentation.mjs";

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
  await reportProblemsInDocumentsAsync(documents);
}

async function reportProblemsInDocumentsAsync(documents) {
  let foundProblems = [];
  for (let doc of documents) {
    await doc.findDiagnosticsAsync();
    foundProblems.push(...doc.findProblems());
  }
  if (foundProblems.length !== 0) {
    throw new ProblemsError(
      `found problems in error documents:\n${foundProblems.join("\n")}`
    );
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
