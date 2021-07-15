// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

"use strict";

const assert = require("assert");
const coc = require("coc.nvim");
const {
  DiagnosticSeverity,
  ProcessCrashed,
  createProcessFactoryAsync,
} = require("quick-lint-js-wasm/quick-lint-js.js");

const source = "quick-lint-js";

// TODO(strager): Allow developers to reload the .wasm file.
const processFactoryPromise = createProcessFactoryAsync();

const DocumentLinterState = {
  // A Parser hasn't been created yet.
  NO_PARSER: "NO_PARSER",

  // A Parser is in the process of being created.
  CREATING_PARSER: "CREATING_PARSER",

  // A Parser has been created, but it has no text.
  PARSER_UNINITIALIZED: "PARSER_UNINITIALIZED",

  // A Parser has been created, and its text is up-to-date with the Document.
  PARSER_LOADED: "PARSER_LOADED",

  // The Parser's Process crashed, and we are creating a new Process and Parser.
  //
  // Document changes should be queued.
  RECOVERING: "RECOVERING",

  DISPOSED: "DISPOSED",
};

class LintingCrashed extends Error {
  constructor(originalError) {
    super(String(originalError));
    this.originalError = originalError;
  }
}

class DocumentLinterDisposed extends Error {}

class DocumentLinter {
  constructor(document, diagnosticCollection) {
    this._document = document;
    this._diagnosticCollection = diagnosticCollection;
    this._state = DocumentLinterState.NO_PARSER;

    // Used only in states: CREATING_PARSER
    this._parserPromise = null;

    // Used only in states: PARSER_UNINITIALIZED, PARSER_LOADED
    this._parser = null;

    // Used only in states: PARSER_LOADED, RECOVERING
    this._pendingChanges = [];

    // Used only in states: RECOVERING
    this._recoveryPromise = null;
  }

  async _createParser() {
    assert.strictEqual(this._state, DocumentLinterState.NO_PARSER);
    this._state = DocumentLinterState.CREATING_PARSER;
    this._parserPromise = (async () => {
      const factory = await processFactoryPromise;
      // TODO(strager): Reuse processes across documents.
      const process = await factory.createProcessAsync();
      const parser = await process.createParserForVSCodeAsync();

      if (this._state === DocumentLinterState.DISPOSED) {
        parser.dispose();
        throw new DocumentLinterDisposed();
      }
      assert.strictEqual(this._state, DocumentLinterState.CREATING_PARSER);
      this._parser = parser;
      this._state = DocumentLinterState.PARSER_UNINITIALIZED;
      return parser;
    })();

    return await this._parserPromise;
  }

  async disposeAsync() {
    const oldState = this._state;
    this._state = DocumentLinterState.DISPOSED;
    switch (oldState) {
      case DocumentLinterState.NO_PARSER:
        break;

      case DocumentLinterState.CREATING_PARSER:
      case DocumentLinterState.PARSER_UNINITIALIZED:
      case DocumentLinterState.PARSER_LOADED: {
        try {
          await this._parserPromise;
        } catch (e) {
          if (!(e instanceof DocumentLinterDisposed)) {
            throw e;
          }
        }
        if (this._parser !== null) {
          this._parser.dispose();
        }
        break;
      }

      case DocumentLinterState.DISPOSED:
        // TODO(strager): Should double-dispose be okay?
        throw new DocumentLinterDisposed();

      default:
        throw new Error(`Unexpected linter state: ${this._state}`);
    }
    this._diagnosticCollection.delete(this._document.uri);
  }

  dispose() {
    logAsyncErrors(async () => {
      await this.disposeAsync();
    });
  }

  async run() {
    switch (this._state) {
      case DocumentLinterState.NO_PARSER:
        await this._createParser();
        await this.run();
        break;

      case DocumentLinterState.CREATING_PARSER:
        await this._parserPromise;
        await this.run();
        break;

      case DocumentLinterState.PARSER_UNINITIALIZED:
        await this._initializeParserAsync();
        break;

      case DocumentLinterState.PARSER_LOADED:
        // No changes could have been made with the editor closed. Ignore.
        break;

      case DocumentLinterState.DISPOSED:
        throw new DocumentLinterDisposed();

      default:
        throw new Error(`Unexpected linter state: ${this._state}`);
    }
  }

  async textChanged(changes) {
    // BEGIN CRITICAL SECTION (no awaiting below)
    switch (this._state) {
      case DocumentLinterState.NO_PARSER:
        // END CRITICAL SECTION (no awaiting above)
        await this._createParser();
        await this._initializeParserAsync();
        break;

      case DocumentLinterState.CREATING_PARSER:
        // END CRITICAL SECTION (no awaiting above)
        await this._parserPromise;
        await this._initializeParserAsync();
        break;

      case DocumentLinterState.PARSER_UNINITIALIZED:
        // END CRITICAL SECTION (no awaiting above)
        await this._initializeParserAsync();
        break;

      case DocumentLinterState.PARSER_LOADED:
        this._pendingChanges.push(...changes);
        try {
          for (const change of this._pendingChanges) {
            this._parser.replaceText(change.range, change.text);
          }
          this._pendingChanges.length = 0;
          // END CRITICAL SECTION (no awaiting above)

          const diags = this._parser.lint();
          this._updateDocumentDiagnostics(diags);
        } catch (e) {
          // END CRITICAL SECTION (no awaiting above)
          if (e instanceof ProcessCrashed) {
            await this._recoverFromCrashAsync(e);
          } else {
            throw e;
          }
        }
        break;

      case DocumentLinterState.RECOVERING:
        this._pendingChanges.push(...changes);
        // END CRITICAL SECTION (no awaiting above)
        await this._recoveryPromise;
        // Changes should have been applied during recovery.
        assert.strictEqual(this._pendingChanges.includes(changes[0]), false);
        break;

      case DocumentLinterState.DISPOSED:
        throw new DocumentLinterDisposed();

      default:
        throw new Error(`Unexpected linter state: ${this._state}`);
    }
  }

  // Transition: PARSER_UNINITIALIZED -> PARSER_LOADED (or NO_PARSER on error)
  async _initializeParserAsync() {
    // BEGIN CRITICAL SECTION (no awaiting below)
    assert.strictEqual(this._state, DocumentLinterState.PARSER_UNINITIALIZED);
    try {
      this._parser.replaceText(
        {
          start: { line: 0, character: 0 },
          end: { line: 0, character: 0 },
        },
        this._document.getText()
      );
      this._pendingChanges.length = 0;
      this._state = DocumentLinterState.PARSER_LOADED;
      // END CRITICAL SECTION (no awaiting above)

      const diags = this._parser.lint();
      this._updateDocumentDiagnostics(diags);
    } catch (e) {
      if (e instanceof ProcessCrashed) {
        await this._recoverFromCrashAsync(e);
      } else {
        throw e;
      }
    }
  }

  // Transition: any -> RECOVERING -> PARSER_LOADED (or NO_PARSER on error)
  async _recoverFromCrashAsync(error) {
    // BEGIN CRITICAL SECTION (no awaiting below)
    console.warn(
      `[${source}] warning: Parser process crashed. Recovering: ${error.stack}`
    );
    this._state = DocumentLinterState.RECOVERING;
    this._recoveryPromise = (async () => {
      let diags;
      try {
        // TODO(strager): Reuse processes across documents.
        const factory = await processFactoryPromise;
        const process = await factory.createProcessAsync();
        const parser = await process.createParserForVSCodeAsync();

        // BEGIN CRITICAL SECTION (no awaiting below)
        assert.strictEqual(this._state, DocumentLinterState.RECOVERING);
        parser.replaceText(
          {
            start: { line: 0, character: 0 },
            end: { line: 0, character: 0 },
          },
          this._document.getText()
        );
        this._pendingChanges.length = 0;
        this._parser = parser;
        this._state = DocumentLinterState.PARSER_LOADED;
        // END CRITICAL SECTION (no awaiting above)

        diags = parser.lint();
      } catch (e) {
        this._parser = null;
        this._parserPromise = null;
        this._state = DocumentLinterState.NO_PARSER;
        if (e instanceof ProcessCrashed) {
          throw new LintingCrashed(e);
        } else {
          throw e;
        }
      }
      this._updateDocumentDiagnostics(diags);
    })();
    // END CRITICAL SECTION (no awaiting above)
    await this._recoveryPromise;
  }

  _updateDocumentDiagnostics(qljsDiagnostics) {
    const diagnostics = qljsDiagnostics.map((diag) => {
      let vsCodeSeverity;
      switch (diag.severity) {
        case DiagnosticSeverity.ERROR:
          vsCodeSeverity = coc.DiagnosticSeverity.Error;
          break;
        case DiagnosticSeverity.WARNING:
          vsCodeSeverity = coc.DiagnosticSeverity.Warning;
          break;
        default:
          console.warn(
            `[${source}] warning: unexpected severity: ${diag.severity}`
          );
          vsCodeSeverity = coc.DiagnosticSeverity.Warning;
          break;
      }
      const range = coc.Range.create(
        diag.startLine,
        diag.startCharacter,
        diag.endLine,
        diag.endCharacter
      );
      const vsCodeDiag = coc.Diagnostic.create(
        range,
        diag.message,
        vsCodeSeverity,
        diag.code,
        source
      );
      return vsCodeDiag;
    });

    this._diagnosticCollection.set(this._document.uri, diagnostics);
  }
}

class DocumentLinterCollection {
  constructor(diagnosticCollection) {
    this._diagnosticCollection = diagnosticCollection;
    this._linters = new Map();
  }

  getLinter(document) {
    const documentURIString = document.uri.toString();
    let linter = this._linters.get(documentURIString);
    if (!linter) {
      linter = new DocumentLinter(document, this._diagnosticCollection);
      this._linters.set(documentURIString, linter);
    }
    return linter;
  }

  async disposeLinter(document) {
    const documentURIString = document.uri.toString();
    const linter = this._linters.get(documentURIString);
    if (typeof linter !== "undefined") {
      await linter.disposeAsync();
      this._linters.delete(documentURIString);
    }
  }
}

exports.activate = async (context) => {
  const diagnosticCollection = coc.languages.createDiagnosticCollection(source);
  const linters = new DocumentLinterCollection(diagnosticCollection);

  coc.workspace.documents.map(async (doc) => {
    if (isLintable(doc.textDocument)) {
      await linters.getLinter(doc.textDocument).run();
    }
  });

  context.subscriptions.push(
    diagnosticCollection,

    coc.workspace.onDidOpenTextDocument(async (document) => {
      if (isLintable(document)) {
        await linters.getLinter(document).run();
      }
    }),

    coc.workspace.onDidSaveTextDocument(async (document) => {
      if (isLintable(document)) {
        await linters.getLinter(document).run();
      }
    }),

    coc.workspace.onDidChangeTextDocument(async (p) => {
      const doc = coc.workspace.getDocument(p.bufnr);
      if (isLintable(doc.textDocument)) {
        await linters.getLinter(doc.textDocument).textChanged(p.contentChanges);
      }
    }),

    coc.workspace.onDidCloseTextDocument(async (document) => {
      await linters.disposeLinter(document);
    })
  );
};

function isLintable(document) {
  return document.languageId === "javascript";
}

function logAsyncErrors(promise) {
  if (typeof promise === "function") {
    promise = promise();
  }
  return promise.catch((error) => {
    const message = error && error.stack ? error.stack : String(error);
    console.error(`[${source}] error: ${message}`);
  });
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
