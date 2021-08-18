// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

"use strict";

let os = require("os");
let path = require("path");
let vscode = require("vscode");

let qljs = require(path.join(
  __dirname,
  `./dist/quick-lint-js-vscode-node_${os.platform()}-${os.arch()}.node`
));

let toDispose = [];

async function activateAsync() {
  let diagnostics = vscode.languages.createDiagnosticCollection();
  toDispose.push(diagnostics);

  let workspace = qljs.createWorkspace({
    vscode: vscode,
    vscodeDiagnosticCollection: diagnostics,
  });
  toDispose.push(workspace);

  toDispose.push(
    vscode.workspace.onDidChangeTextDocument((event) => {
      /*
      Event fires twice on first character input event of the open file. 
      First one is caused by dirty-state change and the second is the actual character change.
      isBogusEvent ensures that event only fires on second event trigger. 
      Implementation is a suggested workaround by maintainer: https://github.com/Microsoft/vscode/issues/50344
      */
      let isBogusEvent = event.contentChanges.length === 0;
      if (!isBogusEvent) {
        logErrors(() => {
          workspace.documentChanged(event.document, event.contentChanges);
        });
      }
    })
  );

  toDispose.push(
    vscode.window.onDidChangeVisibleTextEditors((editors) => {
      logErrors(() => {
        lintVisibleEditors();
      });
    })
  );

  toDispose.push(
    vscode.workspace.onDidCloseTextDocument((vscodeDocument) => {
      logErrors(() => {
        workspace.closeDocument(vscodeDocument);
      });
    })
  );

  function lintVisibleEditors() {
    for (let editor of vscode.window.visibleTextEditors) {
      workspace.editorVisibilityChanged(editor.document);
    }
  }

  lintVisibleEditors();
}
exports.activate = activateAsync;

async function deactivateAsync() {
  let disposing = toDispose.splice(0, toDispose.length);
  disposing.reverse();
  for (let disposable of disposing) {
    await disposable.dispose();
  }
}
exports.deactivate = deactivateAsync;

function logErrors(callback) {
  try {
    callback();
  } catch (error) {
    logError(error);
  }
}

function logError(error) {
  let message = error && error.stack ? error.stack : String(error);
  console.error("[quick-lint-js] error:", message);
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
