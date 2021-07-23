// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

"use strict";

let vscode = require("vscode");
let {
  DiagnosticSeverity,
  DocumentLinter,
  createProcessFactoryAsync,
} = require("quick-lint-js-wasm/quick-lint-js.js");

// TODO(strager): Allow developers to reload the .wasm file.
let processFactoryPromise = createProcessFactoryAsync();

class VSCodeDocumentLinter {
  constructor(document, diagnosticCollection) {
    this._documentLinter = new DocumentLinter(
      {
        getText() {
          return document.getText();
        },
        setDiagnostics(qljsDiagnostics) {
          let vscodeDiagnostics = qljsDiagnostics.map((diag) =>
            VSCodeDocumentLinter._qljsDiagnosticToVSCodeDiagnostic(diag)
          );
          diagnosticCollection.set(document.uri, vscodeDiagnostics);
        },
        removeDiagnostics() {
          diagnosticCollection.delete(document.uri);
        },
      },
      processFactoryPromise
    );
  }

  async disposeAsync() {
    await this._documentLinter.disposeAsync();
  }

  dispose() {
    this._documentLinter.dispose();
  }

  async editorChangedVisibilityAsync() {
    await this._documentLinter.editorChangedVisibilityAsync();
  }

  async textChangedAsync(changes) {
    await this._documentLinter.textChangedAsync(changes);
  }

  static _qljsDiagnosticToVSCodeDiagnostic(diag) {
    let vsCodeSeverity;
    switch (diag.severity) {
      case DiagnosticSeverity.ERROR:
        vsCodeSeverity = vscode.DiagnosticSeverity.Error;
        break;
      case DiagnosticSeverity.WARNING:
        vsCodeSeverity = vscode.DiagnosticSeverity.Warning;
        break;
      default:
        console.warn(
          `[quick-lint-js] warning: unexpected severity: ${diag.severity}`
        );
        vsCodeSeverity = vscode.DiagnosticSeverity.Warning;
        break;
    }
    let vsCodeDiag = new vscode.Diagnostic(
      new vscode.Range(
        new vscode.Position(diag.startLine, diag.startCharacter),
        new vscode.Position(diag.endLine, diag.endCharacter)
      ),
      diag.message,
      vsCodeSeverity
    );
    vsCodeDiag.code = diag.code;
    vsCodeDiag.source = "quick-lint-js";
    return vsCodeDiag;
  }
}

class DocumentLinterCollection {
  constructor(diagnosticCollection) {
    this._diagnosticCollection = diagnosticCollection;

    // Mapping from URI string to VSCodeDocumentLinter.
    this._linters = new Map();
  }

  getLinter(document) {
    let documentURIString = document.uri.toString();
    let linter = this._linters.get(documentURIString);
    if (typeof linter === "undefined") {
      linter = new VSCodeDocumentLinter(document, this._diagnosticCollection);
      this._linters.set(documentURIString, linter);
    }
    return linter;
  }

  async disposeLinterAsync(document) {
    let documentURIString = document.uri.toString();
    let linter = this._linters.get(documentURIString);
    if (typeof linter !== "undefined") {
      await linter.disposeAsync();
      this._linters.delete(documentURIString);
    }
  }

  async disposeAsync() {
    this._linters = new Map();
    for (let [_uri, linter] of this._linters) {
      await linter.disposeAsync();
    }
  }

  dispose() {
    logAsyncErrors(async () => {
      await this.disposeAsync();
    });
  }
}
exports.DocumentLinterCollection = DocumentLinterCollection;

let toDispose = [];

async function activateAsync() {
  let diagnostics = vscode.languages.createDiagnosticCollection();
  toDispose.push(diagnostics);

  let linters = new DocumentLinterCollection(diagnostics);
  toDispose.push(linters);

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
        logAsyncErrors(
          (async () => {
            if (isLintable(event.document)) {
              await linters
                .getLinter(event.document)
                .textChangedAsync(event.contentChanges);
            }
          })()
        );
      }
    })
  );

  toDispose.push(
    vscode.window.onDidChangeVisibleTextEditors((editors) => {
      logAsyncErrors(async () => {
        await lintVisibleEditorsAsync();
      });
    })
  );

  toDispose.push(
    vscode.workspace.onDidCloseTextDocument((document) => {
      logAsyncErrors(async () => {
        await linters.disposeLinterAsync(document);
      });
    })
  );

  async function lintVisibleEditorsAsync() {
    for (let editor of vscode.window.visibleTextEditors) {
      if (isLintable(editor.document)) {
        await linters.getLinter(editor.document).editorChangedVisibilityAsync();
      }
    }
  }

  await lintVisibleEditorsAsync();
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

function isLintable(document) {
  return document.languageId === "javascript";
}

function logAsyncErrors(promise) {
  if (typeof promise === "function") {
    promise = promise();
  }
  return promise.catch((error) => {
    logError(error);
  });
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
