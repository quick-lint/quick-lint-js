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

class QLJSDocument {
  constructor(qljsWorkspace, document, diagnosticCollection) {
    this._document = document;
    this._diagnosticCollection = diagnosticCollection;
    this._qljsDocument = qljsWorkspace.createDocument();
  }

  dispose() {
    this._diagnosticCollection.delete(this._document.uri);
    this._qljsDocument.dispose();
  }

  editorChangedVisibility() {
    this._qljsDocument.setText(this._document.getText());
    this._lint();
  }

  textChanged(changes) {
    this._qljsDocument.replaceText(changes);
    this._lint();
  }

  _lint() {
    let vscodeDiagnostics = this._qljsDocument.lint();
    this._diagnosticCollection.set(this._document.uri, vscodeDiagnostics);
  }
}

class QLJSWorkspace {
  constructor(diagnosticCollection) {
    this._diagnosticCollection = diagnosticCollection;
    this._qljsWorkspace = qljs.createWorkspace(vscode);

    // Mapping from URI string to QLJSDocument.
    this._qljsDocuments = new Map();
  }

  getLinter(document) {
    let documentURIString = document.uri.toString();
    let qljsDocument = this._qljsDocuments.get(documentURIString);
    if (typeof qljsDocument === "undefined") {
      qljsDocument = new QLJSDocument(
        this._qljsWorkspace,
        document,
        this._diagnosticCollection
      );
      this._qljsDocuments.set(documentURIString, qljsDocument);
    }
    return qljsDocument;
  }

  disposeLinter(document) {
    let documentURIString = document.uri.toString();
    let qljsDocument = this._qljsDocuments.get(documentURIString);
    if (typeof qljsDocument !== "undefined") {
      qljsDocument.dispose();
      this._qljsDocuments.delete(documentURIString);
    }
  }

  dispose() {
    let qljsDocuments = this._qljsDocuments;
    this._qljsDocuments = new Map();
    for (let [_uri, qljsDocument] of qljsDocuments) {
      qljsDocument.dispose();
    }
  }
}
exports.QLJSWorkspace = QLJSWorkspace;

let toDispose = [];

async function activateAsync() {
  let diagnostics = vscode.languages.createDiagnosticCollection();
  toDispose.push(diagnostics);

  let qljsWorkspace = new QLJSWorkspace(diagnostics);
  toDispose.push(qljsWorkspace);

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
          if (isLintable(event.document)) {
            qljsWorkspace
              .getLinter(event.document)
              .textChanged(event.contentChanges);
          }
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
    vscode.workspace.onDidCloseTextDocument((document) => {
      logErrors(() => {
        qljsWorkspace.disposeLinter(document);
      });
    })
  );

  function lintVisibleEditors() {
    for (let editor of vscode.window.visibleTextEditors) {
      if (isLintable(editor.document)) {
        qljsWorkspace.getLinter(editor.document).editorChangedVisibility();
      }
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

function isLintable(document) {
  return document.languageId === "javascript";
}

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
