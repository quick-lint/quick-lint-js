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

class AbstractDocument {
  constructor(workspace, vscodeDocument, diagnosticCollection) {
    this._vscodeDocument = vscodeDocument;
    this._diagnosticCollection = diagnosticCollection;
    this._qljsDocument = workspace.createDocument(
      vscodeDocument,
      diagnosticCollection,
      /*isConfigFile=*/ this.constructor === ConfigDocument
    );
    this._qljsDocument._document = this;
  }

  dispose() {
    this._qljsDocument.dispose();
  }

  editorChangedVisibility() {
    this._qljsDocument.setText(this._vscodeDocument.getText());
  }

  textChanged(changes) {
    this._qljsDocument.replaceText(changes);
  }
}

class ConfigDocument extends AbstractDocument {}

class LintableDocument extends AbstractDocument {}

class Workspace {
  constructor(diagnosticCollection) {
    this._diagnosticCollection = diagnosticCollection;
    this._qljsWorkspace = qljs.createWorkspace({
      vscode: vscode,
    });

    // Mapping from URI string to Document.
    this._documents = new Map();
  }

  // Returns null if no associated linter exists.
  getExistingLinter(vscodeDocument) {
    let documentURIString = vscodeDocument.uri.toString();
    let document = this._documents.get(documentURIString);
    if (typeof document === "undefined") {
      return null;
    }
    return document;
  }

  // Throws if an associated linter already exists (i.e. if
  // getExistingLinter(vscodeDocument) returns non-null).
  createLinter(vscodeDocument, documentType) {
    let documentURIString = vscodeDocument.uri.toString();
    if (this._documents.has(documentURIString)) {
      throw new Error(
        `Document already created for vscode.Document ${documentURIString}`
      );
    }
    let document = new documentType(
      this._qljsWorkspace,
      vscodeDocument,
      this._diagnosticCollection
    );
    this._documents.set(documentURIString, document);
    return document;
  }

  disposeLinter(vscodeDocument) {
    let documentURIString = vscodeDocument.uri.toString();
    let document = this._documents.get(documentURIString);
    if (typeof document !== "undefined") {
      document.dispose();
      this._documents.delete(documentURIString);
    }
  }

  dispose() {
    let documents = this._documents;
    this._documents = new Map();
    for (let [_uri, document] of documents) {
      document.dispose();
    }
  }

  // Returns a subclass of AbstractDocument (the class itself, not an instance).
  //
  // Returns null if this extension should ignore the document.
  classifyDocument(vscodeDocument) {
    if (vscodeDocument.languageId === "javascript") {
      return LintableDocument;
    }
    if (
      vscodeDocument.uri.scheme === "file" &&
      this._qljsWorkspace.isConfigFilePath(vscodeDocument.uri.fsPath)
    ) {
      return ConfigDocument;
    }
    return null;
  }
}
exports.Workspace = Workspace;

let toDispose = [];

async function activateAsync() {
  let diagnostics = vscode.languages.createDiagnosticCollection();
  toDispose.push(diagnostics);

  let workspace = new Workspace(diagnostics);
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
          let document = workspace.getExistingLinter(event.document);
          if (document !== null) {
            document.textChanged(event.contentChanges);
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
    vscode.workspace.onDidCloseTextDocument((vscodeDocument) => {
      logErrors(() => {
        workspace.disposeLinter(vscodeDocument);
      });
    })
  );

  function lintVisibleEditors() {
    for (let editor of vscode.window.visibleTextEditors) {
      let vscodeDocument = editor.document;
      let document = workspace.getExistingLinter(vscodeDocument);
      if (document === null) {
        let documentType = workspace.classifyDocument(vscodeDocument);
        if (documentType !== null) {
          document = workspace.createLinter(vscodeDocument, documentType);
        }
      }
      if (document !== null) {
        document.editorChangedVisibility();
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
