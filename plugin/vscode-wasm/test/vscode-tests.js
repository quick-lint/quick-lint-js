// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

"use strict";

if (require.main === module) {
  console.error(
    `fatal: do not execute ${__filename} directly; run run-vscode-tests.js instead`
  );
  process.exit(1);
}

let assert = require("assert");
let fs = require("fs");
let os = require("os");
let path = require("path");
let { testMainAsync } = require("./test-support.js");

let vscode = require("vscode");
let qljsExtension = require("../extension.js");

let tests = {};

for (let extension of [".js", ".mjs", ".cjs"]) {
  tests = {
    ...tests,

    [`load plugin after file opened (${extension})`]: async ({
      addCleanup,
    }) => {
      let scratchDirectory = makeScratchDirectory({ addCleanup });
      let helloFilePath = path.join(scratchDirectory, `hello${extension}`);
      fs.writeFileSync(helloFilePath, "let x = 3;\nlet x = 4;\n");
      let helloURI = vscode.Uri.file(helloFilePath);

      let helloDocument = await vscode.workspace.openTextDocument(helloURI);
      let helloEditor = await vscode.window.showTextDocument(helloDocument);
      await loadExtensionAsync({ addCleanup });

      await waitUntilAnyDiagnosticsAsync(helloURI);
    },

    [`open existing file ${extension}`]: async ({ addCleanup }) => {
      let scratchDirectory = makeScratchDirectory({ addCleanup });
      let helloFilePath = path.join(scratchDirectory, `hello${extension}`);
      fs.writeFileSync(helloFilePath, "let x = 3;\nlet x = 4;\n");
      let helloURI = vscode.Uri.file(helloFilePath);

      await loadExtensionAsync({ addCleanup });
      let helloDocument = await vscode.workspace.openTextDocument(helloURI);
      let helloEditor = await vscode.window.showTextDocument(helloDocument);

      await pollAsync(async () => {
        let helloDiags = normalizeDiagnostics(helloURI);
        assert.deepStrictEqual(helloDiags, [
          {
            code: "E034",
            message: "redeclaration of variable: x",
            severity: vscode.DiagnosticSeverity.Error,
            source: "quick-lint-js",
            startLine: 1,
            startCharacter: 4,
            endLine: 1,
            endCharacter: 5,
          },
        ]);
      });
    },
  };
}

tests = {
  ...tests,

  "load plugin after non-JS file opened": async ({ addCleanup }) => {
    let scratchDirectory = makeScratchDirectory({ addCleanup });
    let helloFilePath = path.join(scratchDirectory, "hello.txt");
    fs.writeFileSync(helloFilePath, "let x = 3;\nlet x = 4;\n");
    let helloURI = vscode.Uri.file(helloFilePath);

    let helloDocument = await vscode.workspace.openTextDocument(helloURI);
    let helloEditor = await vscode.window.showTextDocument(helloDocument);
    await loadExtensionAsync({ addCleanup });

    // Wait for possible linting to take effect.
    await sleepAsync(100);

    await waitUntilNoDiagnosticsAsync(helloURI);
  },

  "open existing non-JS file": async ({ addCleanup }) => {
    let scratchDirectory = makeScratchDirectory({ addCleanup });
    let helloFilePath = path.join(scratchDirectory, "hello.txt");
    fs.writeFileSync(helloFilePath, "let x = 3;\nlet x = 4;\n");
    let helloURI = vscode.Uri.file(helloFilePath);

    await loadExtensionAsync({ addCleanup });
    let helloDocument = await vscode.workspace.openTextDocument(helloURI);
    let helloEditor = await vscode.window.showTextDocument(helloDocument);

    // Wait for possible linting to take effect.
    await sleepAsync(100);

    await waitUntilNoDiagnosticsAsync(helloURI);
  },

  "rename open .js file to .py": async ({ addCleanup }) => {
    let scratchDirectory = makeScratchDirectory({ addCleanup });
    let jsFilePath = path.join(scratchDirectory, "hello.js");
    fs.writeFileSync(jsFilePath, "let x = 3;\nlet x = 4;\n");
    let jsURI = vscode.Uri.file(jsFilePath);

    await loadExtensionAsync({ addCleanup });
    let helloDocument = await vscode.workspace.openTextDocument(jsURI);
    let helloEditor = await vscode.window.showTextDocument(helloDocument);
    await waitUntilAnyDiagnosticsAsync(jsURI);

    let pyFilePath = path.join(scratchDirectory, "hello.py");
    let pyURI = vscode.Uri.file(pyFilePath);
    let edits = new vscode.WorkspaceEdit();
    edits.renameFile(jsURI, pyURI);
    let edited = await vscode.workspace.applyEdit(edits);
    assert.strictEqual(edited, true);

    await waitUntilNoDiagnosticsAsync(jsURI);
    await waitUntilNoDiagnosticsAsync(pyURI);
  },

  "rename open .py file to .js": async ({ addCleanup }) => {
    let scratchDirectory = makeScratchDirectory({ addCleanup });
    let pyFilePath = path.join(scratchDirectory, "hello.py");
    fs.writeFileSync(pyFilePath, "let x = 3;\nlet x = 4;\n");
    let pyURI = vscode.Uri.file(pyFilePath);

    await loadExtensionAsync({ addCleanup });
    let helloDocument = await vscode.workspace.openTextDocument(pyURI);
    let helloEditor = await vscode.window.showTextDocument(helloDocument);

    // Wait for possible linting to take effect.
    await sleepAsync(100);
    await waitUntilNoDiagnosticsAsync(pyURI);

    let jsFilePath = path.join(scratchDirectory, "hello.js");
    let jsURI = vscode.Uri.file(jsFilePath);
    let edits = new vscode.WorkspaceEdit();
    edits.renameFile(pyURI, jsURI);
    let edited = await vscode.workspace.applyEdit(edits);
    assert.strictEqual(edited, true);

    await waitUntilAnyDiagnosticsAsync(jsURI);
  },

  "file on disk changes": async ({ addCleanup }) => {
    let scratchDirectory = makeScratchDirectory({ addCleanup });
    let helloFilePath = path.join(scratchDirectory, "hello.js");
    fs.writeFileSync(helloFilePath, "/* empty file */\n");
    let helloURI = vscode.Uri.file(helloFilePath);

    await loadExtensionAsync({ addCleanup });
    let helloDocument = await vscode.workspace.openTextDocument(helloURI);
    let helloEditor = await vscode.window.showTextDocument(helloDocument);

    // HACK(strager): Wait for VS Code to register its filesystem watchers.
    await sleepAsync(100);

    fs.writeFileSync(helloFilePath, "let x = 3;\nlet x = 4;\n");

    await pollAsync(async () => {
      let helloDiags = normalizeDiagnostics(helloURI);
      assert.deepStrictEqual(helloDiags, [
        {
          code: "E034",
          message: "redeclaration of variable: x",
          severity: vscode.DiagnosticSeverity.Error,
          source: "quick-lint-js",
          startLine: 1,
          startCharacter: 4,
          endLine: 1,
          endCharacter: 5,
        },
      ]);
    });
  },

  "inline edits of existing document": async ({ addCleanup }) => {
    let scratchDirectory = makeScratchDirectory({ addCleanup });
    let helloFilePath = path.join(scratchDirectory, "hello.js");
    fs.writeFileSync(
      helloFilePath,
      "let classy = true;\nconsole.log(clas\\u{73});\n"
    );
    let helloURI = vscode.Uri.file(helloFilePath);

    let helloDocument = await vscode.workspace.openTextDocument(helloURI);
    let helloEditor = await vscode.window.showTextDocument(helloDocument);
    await loadExtensionAsync({ addCleanup });

    // clas\u{73} is illegal. We should get a lint error.
    await waitUntilAnyDiagnosticsAsync(helloURI);

    let didApplyEdits = await helloEditor.edit((editBuilder) => {
      // clas\u{73} -> clas\u{73}y
      editBuilder.insert(new vscode.Position(1, 16), "sy");
      editBuilder.delete(
        new vscode.Range(
          new vscode.Position(1, 16),
          new vscode.Position(1, 16 + 6)
        )
      );
    });
    assert.strictEqual(didApplyEdits, true);

    await waitUntilNoDiagnosticsAsync(helloURI);
  },

  "inline edits of new document": async ({ addCleanup }) => {
    await loadExtensionAsync({ addCleanup });
    let helloURI = vscode.Uri.parse("untitled:hello.js");
    let helloDocument = await vscode.workspace.openTextDocument(helloURI);
    let helloEditor = await vscode.window.showTextDocument(helloDocument);

    let didApplyEdits = await helloEditor.edit((editBuilder) => {
      editBuilder.insert(
        new vscode.Position(0, 0),
        "console.log(\\u{ffffffff});"
      );
    });
    assert.strictEqual(didApplyEdits, true);

    await waitUntilAnyDiagnosticsAsync(helloURI);
  },

  "inline edits of .txt document are ignored": async ({ addCleanup }) => {
    await loadExtensionAsync({ addCleanup });
    let helloURI = vscode.Uri.parse("untitled:hello.txt");
    let helloDocument = await vscode.workspace.openTextDocument(helloURI);
    let helloEditor = await vscode.window.showTextDocument(helloDocument);

    let didApplyEdits = await helloEditor.edit((editBuilder) => {
      editBuilder.insert(new vscode.Position(0, 0), "let x; let x;");
    });
    assert.strictEqual(didApplyEdits, true);

    // Wait for possible linting to take effect.
    await sleepAsync(100);

    await waitUntilNoDiagnosticsAsync(helloURI);
  },

  "separate documents are linted separately": async ({ addCleanup }) => {
    await loadExtensionAsync({ addCleanup });

    let leftURI = vscode.Uri.parse("untitled:left.js");
    let leftDocument = await vscode.workspace.openTextDocument(leftURI);
    let leftEditor = await vscode.window.showTextDocument(
      leftDocument,
      vscode.ViewColumn.One
    );

    let rightURI = vscode.Uri.parse("untitled:right.js");
    let rightDocument = await vscode.workspace.openTextDocument(rightURI);
    let rightEditor = await vscode.window.showTextDocument(
      rightDocument,
      vscode.ViewColumn.Two
    );

    await leftEditor.edit((editBuilder) => {
      editBuilder.insert(new vscode.Position(0, 0), "console.log(");
    });
    await rightEditor.edit((editBuilder) => {
      editBuilder.insert(new vscode.Position(0, 0), "let x = ");
    });
    await leftEditor.edit((editBuilder) => {
      editBuilder.insert(
        new vscode.Position(0, "console.log(".length),
        "undeclaredLeftVariable);"
      );
    });
    await rightEditor.edit((editBuilder) => {
      editBuilder.insert(
        new vscode.Position(0, "let x = ".length),
        "undeclaredRightVariable;"
      );
    });

    await pollAsync(async () => {
      let leftDiags = normalizeDiagnostics(leftURI);
      assert.deepStrictEqual(
        leftDiags.map((diag) => diag.message),
        ["use of undeclared variable: undeclaredLeftVariable"]
      );
      let rightDiags = normalizeDiagnostics(rightURI);
      assert.deepStrictEqual(
        rightDiags.map((diag) => diag.message),
        ["use of undeclared variable: undeclaredRightVariable"]
      );
    });
  },

  "existing documents without editors are not linted": async ({
    addCleanup,
  }) => {
    let scratchDirectory = makeScratchDirectory({ addCleanup });
    let helloFilePath = path.join(scratchDirectory, "hello.js");
    fs.writeFileSync(helloFilePath, "let x = 3;\nlet x = 4;\n");
    let helloURI = vscode.Uri.file(helloFilePath);

    let helloDocument = await vscode.workspace.openTextDocument(helloURI);
    // Do not call showTextDocument.
    await loadExtensionAsync({ addCleanup });

    // Wait for possible linting to take effect.
    await sleepAsync(100);

    await waitUntilNoDiagnosticsAsync(helloURI);
  },

  "changing previously-edited document while editor is closed lints when reopening editor":
    async ({ addCleanup }) => {
      let scratchDirectory = makeScratchDirectory({ addCleanup });
      let helloFilePath = path.join(scratchDirectory, "hello.js");
      fs.writeFileSync(helloFilePath, "/* empty file */\n");
      let helloURI = vscode.Uri.file(helloFilePath);

      let helloDocument = await vscode.workspace.openTextDocument(helloURI);
      let _originalHelloEditor = await vscode.window.showTextDocument(
        helloDocument,
        vscode.ViewColumn.One
      );
      await loadExtensionAsync({ addCleanup });
      // Wait for possible linting to take effect.
      await sleepAsync(100);

      // Close _originalHelloEditor.
      let otherURI = vscode.Uri.parse("untitled:other.txt");
      let otherDocument = await vscode.workspace.openTextDocument(otherURI);
      let _otherEditor = await vscode.window.showTextDocument(
        otherDocument,
        vscode.ViewColumn.One
      );

      fs.writeFileSync(helloFilePath, "let x;let x;");
      // Wait for possible linting to take effect.
      await sleepAsync(100);

      let _newHelloEditor = await vscode.window.showTextDocument(
        helloDocument,
        vscode.ViewColumn.Two
      );
      await waitUntilAnyDiagnosticsAsync(helloURI);
    },

  "opening editor for existing documents lints": async ({ addCleanup }) => {
    let scratchDirectory = makeScratchDirectory({ addCleanup });
    let helloFilePath = path.join(scratchDirectory, "hello.js");
    fs.writeFileSync(helloFilePath, "let x = 3;\nlet x = 4;\n");
    let helloURI = vscode.Uri.file(helloFilePath);

    let helloDocument = await vscode.workspace.openTextDocument(helloURI);
    await loadExtensionAsync({ addCleanup });
    let helloEditor = await vscode.window.showTextDocument(helloDocument);

    await waitUntilAnyDiagnosticsAsync(helloURI);
  },

  "deactivating plugin removes diagnostics": async ({ addCleanup }) => {
    let deactivated = false;
    addCleanup(async () => {
      if (!deactivated) {
        await qljsExtension.deactivate();
      }
    });
    await qljsExtension.activate();

    let scratchDirectory = makeScratchDirectory({ addCleanup });
    let helloFilePath = path.join(scratchDirectory, "hello.js");
    fs.writeFileSync(helloFilePath, "let x = 3;\nlet x = 4;\n");
    let helloURI = vscode.Uri.file(helloFilePath);
    let helloDocument = await vscode.workspace.openTextDocument(helloURI);
    let helloEditor = await vscode.window.showTextDocument(helloDocument);

    await waitUntilAnyDiagnosticsAsync(helloURI);

    await qljsExtension.deactivate();
    deactivated = true;

    await waitUntilNoDiagnosticsAsync(helloURI);
  },
};

async function waitUntilAnyDiagnosticsAsync(documentURI) {
  await pollAsync(async () => {
    let diags = normalizeDiagnostics(documentURI);
    assert.strictEqual(diags.length >= 1, true);
  });
}

async function waitUntilNoDiagnosticsAsync(documentURI) {
  await pollAsync(async () => {
    let diags = normalizeDiagnostics(documentURI);
    assert.deepStrictEqual(diags, []);
  });
}

// Convert an array of vscode.Diagnostic into an array of plain JavaScript
// objects. Use this with assert.deepStrictEqual in tests.
//
// If given a vscode.Uri, diagnostics are extracted from that document.
function normalizeDiagnostics(vscodeDiagnosticsOrURI) {
  let vscodeDiagnostics;
  if (vscodeDiagnosticsOrURI instanceof vscode.Uri) {
    vscodeDiagnostics = vscode.languages.getDiagnostics(vscodeDiagnosticsOrURI);
  } else {
    vscodeDiagnostics = vscodeDiagnosticsOrURI;
  }
  return vscodeDiagnostics.map((diag) => ({
    code: diag.code,
    message: diag.message,
    source: diag.source,
    severity: diag.severity,
    startLine: diag.range.start.line,
    startCharacter: diag.range.start.character,
    endLine: diag.range.end.line,
    endCharacter: diag.range.end.character,
  }));
}

async function loadExtensionAsync({ addCleanup }) {
  addCleanup(async () => {
    await qljsExtension.deactivate();
  });
  await qljsExtension.activate();
}

function makeScratchDirectory({ addCleanup }) {
  let scratchDirectory = fs.mkdtempSync(
    path.join(os.tmpdir(), "quick-lint-js-vscode-test-")
  );
  addCleanup(() => {
    fs.rmdirSync(scratchDirectory, { recursive: true });
  });
  return scratchDirectory;
}

function sleepAsync(duration) {
  return new Promise((resolve, _reject) => {
    setTimeout(() => {
      resolve();
    }, duration);
  });
}

async function pollAsync(callback) {
  let totalMilliseconds = 5000;
  let sleepMilliseconds = 100;
  let totalAttempts = totalMilliseconds / sleepMilliseconds;

  for (let attempt = 0; attempt < totalAttempts - 1; ++attempt) {
    try {
      await callback();
      return;
    } catch (error) {
      if (error instanceof assert.AssertionError) {
        // Fall through.
      } else {
        throw error;
      }
    }
    await sleepAsync(sleepMilliseconds);
  }

  await callback(); // Last attempt.
}

async function runAsync() {
  // vscode-test activated the extension for us. We want tests to be activate and
  // deactivate the extension at will.
  let extension = vscode.extensions.getExtension("quick-lint.quick-lint-js-wasm");
  await extension.activate();
  await qljsExtension.deactivate();

  // Disable features of extensions which might conflict with quick-lint-js.
  // NOTE(strager): We can't completely disable the vscode.javascript extension,
  // because without its JavaScript file detection, our extension doesn't work.
  let vscodeConfig = vscode.workspace.getConfiguration();
  await vscodeConfig.update("javascript.validate.enable", false);

  await testMainAsync(tests, (message) => {
    throw new Error(message);
  });
}
exports.run = runAsync;
// vscode-test will invoke the exports.run for us.

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
