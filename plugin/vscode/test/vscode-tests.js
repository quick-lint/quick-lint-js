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

  "diagnostic severity and code": async ({ addCleanup }) => {
    let scratchDirectory = makeScratchDirectory({ addCleanup });
    let helloFilePath = path.join(scratchDirectory, "hello.js");
    fs.writeFileSync(helloFilePath, "let x = undeclaredVariable;\nlet x;\n");
    let helloURI = vscode.Uri.file(helloFilePath);

    let helloDocument = await vscode.workspace.openTextDocument(helloURI);
    await loadExtensionAsync({ addCleanup });
    let helloEditor = await vscode.window.showTextDocument(helloDocument);

    await waitUntilAnyDiagnosticsAsync(helloURI);
    let diags = normalizeDiagnostics(helloURI);
    diags = diags.map(({ code, severity }) => ({ code, severity }));
    diags.sort((a, b) => {
      if (a.code < b.code) return -1;
      if (a.code > b.code) return +1;
      return 0;
    });
    assert.deepStrictEqual(diags, [
      // redeclaration of variable 'x'
      { code: "E034", severity: vscode.DiagnosticSeverity.Error },
      // use of undeclared variable 'undeclaredVariable'
      { code: "E057", severity: vscode.DiagnosticSeverity.Warning },
    ]);
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

  "opened .js file uses quick-lint-js.config from disk": async ({
    addCleanup,
  }) => {
    let messageMocker = VSCodeMessageMocker.mock({ addCleanup });

    let scratchDirectory = makeScratchDirectory({ addCleanup });
    let jsFilePath = path.join(scratchDirectory, "hello.js");
    fs.writeFileSync(jsFilePath, "testGlobalVariable;\ndocument;");
    let jsURI = vscode.Uri.file(jsFilePath);
    let configFilePath = path.join(scratchDirectory, "quick-lint-js.config");
    fs.writeFileSync(
      configFilePath,
      '{"globals": {"testGlobalVariable": true}, "global-groups": ["ecmascript"]}'
    );

    await loadExtensionAsync({ addCleanup });
    let jsDocument = await vscode.workspace.openTextDocument(jsURI);
    let jsEditor = await vscode.window.showTextDocument(jsDocument);

    await waitUntilAnyDiagnosticsAsync(jsURI);
    let jsDiags = normalizeDiagnostics(jsURI);
    assert.deepStrictEqual(
      jsDiags.map(({ code, startLine }) => ({ code, startLine })),
      [
        {
          code: "E057",
          startLine: 1, // document
        },
      ]
    );

    messageMocker.assertNoMessages();
  },

  "I/O error loading quick-lint-js.config shows pop-up": async ({
    addCleanup,
  }) => {
    let messageMocker = VSCodeMessageMocker.mock({ addCleanup });

    let scratchDirectory = makeScratchDirectory({ addCleanup });
    let jsFilePath = path.join(scratchDirectory, "hello.js");
    fs.writeFileSync(jsFilePath, "variableDoesNotExist;");
    let jsURI = vscode.Uri.file(jsFilePath);
    let configFilePath = path.join(scratchDirectory, "quick-lint-js.config");
    fs.mkdirSync(configFilePath);

    await loadExtensionAsync({ addCleanup });
    let jsDocument = await vscode.workspace.openTextDocument(jsURI);
    let jsEditor = await vscode.window.showTextDocument(jsDocument);

    await waitUntilAnyDiagnosticsAsync(jsURI);

    messageMocker.assertAnyErrorMessageMatches(
      /Failed to load configuration file for .*hello\.js\. Using default configuration\.\nError details: failed to read from .*quick-lint-js\.config: .*/
    );
  },

  "opening .js file with error in quick-lint-js.config shows pop-up": async ({
    addCleanup,
  }) => {
    let messageMocker = VSCodeMessageMocker.mock({ addCleanup });

    let scratchDirectory = makeScratchDirectory({ addCleanup });
    let jsFilePath = path.join(scratchDirectory, "hello.js");
    fs.writeFileSync(jsFilePath, "variableDoesNotExist;");
    let jsURI = vscode.Uri.file(jsFilePath);
    let configFilePath = path.join(scratchDirectory, "quick-lint-js.config");
    fs.writeFileSync(configFilePath, "{SYNTAX ERROR}");

    await loadExtensionAsync({ addCleanup });
    let jsDocument = await vscode.workspace.openTextDocument(jsURI);
    let jsEditor = await vscode.window.showTextDocument(jsDocument);

    await waitUntilAnyDiagnosticsAsync(jsURI);

    messageMocker.assertAnyErrorMessageMatches(
      /Problems found in the config file for .*hello\.js \(.*quick-lint-js\.config\)./
    );
  },

  "clicking Open-Config button in quick-lint-js.config error pop-up opens config file":
    async ({ addCleanup }) => {
      let messageMocker = VSCodeMessageMocker.mock({ addCleanup });

      let scratchDirectory = makeScratchDirectory({ addCleanup });
      let jsFilePath = path.join(scratchDirectory, "hello.js");
      fs.writeFileSync(jsFilePath, "variableDoesNotExist;");
      let jsURI = vscode.Uri.file(jsFilePath);
      let configFilePath = path.join(scratchDirectory, "quick-lint-js.config");
      fs.writeFileSync(configFilePath, "{SYNTAX ERROR}");

      await loadExtensionAsync({ addCleanup });
      let jsDocument = await vscode.workspace.openTextDocument(jsURI);
      let jsEditor = await vscode.window.showTextDocument(jsDocument);

      await messageMocker.waitUntilAnyMessageAsync();
      assert.strictEqual(
        path.basename(vscode.window.activeTextEditor.document.fileName),
        "hello.js"
      );

      messageMocker.getErrorMessages()[0].clickButton("Open config");
      await pollAsync(async () => {
        assert.strictEqual(
          path.basename(vscode.window.activeTextEditor.document.fileName),
          "quick-lint-js.config"
        );
      }, 1000);
    },

  "dismissing quick-lint-js.config error pop-up does not open config file":
    async ({ addCleanup }) => {
      let messageMocker = VSCodeMessageMocker.mock({ addCleanup });

      let scratchDirectory = makeScratchDirectory({ addCleanup });
      let jsFilePath = path.join(scratchDirectory, "hello.js");
      fs.writeFileSync(jsFilePath, "variableDoesNotExist;");
      let jsURI = vscode.Uri.file(jsFilePath);
      let configFilePath = path.join(scratchDirectory, "quick-lint-js.config");
      fs.writeFileSync(configFilePath, "{SYNTAX ERROR}");

      await loadExtensionAsync({ addCleanup });
      let jsDocument = await vscode.workspace.openTextDocument(jsURI);
      let jsEditor = await vscode.window.showTextDocument(jsDocument);

      await messageMocker.waitUntilAnyMessageAsync();
      messageMocker.getErrorMessages()[0].dismiss();
      // Wait for possible opening of quick-lint-js.config to take effect.
      await sleepAsync(100);
      assert.strictEqual(
        path.basename(vscode.window.activeTextEditor.document.fileName),
        "hello.js"
      );
    },

  "opened .js file uses opened quick-lint-js.config (not from disk)": async ({
    addCleanup,
  }) => {
    let scratchDirectory = makeScratchDirectory({ addCleanup });
    let jsFilePath = path.join(scratchDirectory, "hello.js");
    fs.writeFileSync(
      jsFilePath,
      "testGlobalVariableFromEditor;\ntestGlobalVariableFromDisk;"
    );
    let jsURI = vscode.Uri.file(jsFilePath);
    let configFilePath = path.join(scratchDirectory, "quick-lint-js.config");
    fs.writeFileSync(
      configFilePath,
      '{"globals": {"testGlobalVariableFromDisk": true}}'
    );
    let configURI = vscode.Uri.file(configFilePath);

    await loadExtensionAsync({ addCleanup });
    let configDocument = await vscode.workspace.openTextDocument(configURI);
    let configEditor = await vscode.window.showTextDocument(configDocument);
    await configEditor.edit((editBuilder) => {
      editBuilder.replace(
        new vscode.Range(new vscode.Position(0, 0), new vscode.Position(1, 0)),
        '{"globals": {"testGlobalVariableFromEditor": true}, "global-groups": ["ecmascript"]}'
      );
    });

    let jsDocument = await vscode.workspace.openTextDocument(jsURI);
    let jsEditor = await vscode.window.showTextDocument(jsDocument);
    await waitUntilAnyDiagnosticsAsync(jsURI);

    let jsDiags = normalizeDiagnostics(jsURI);
    assert.deepStrictEqual(
      jsDiags.map(({ code, startLine }) => ({ code, startLine })),
      [
        {
          code: "E057",
          startLine: 1, // testGlobalVariableFromDisk
        },
      ]
    );
  },

  "valid quick-lint-js.config has no diagnostics; should not be linted as a .js file":
    async ({ addCleanup }) => {
      let scratchDirectory = makeScratchDirectory({ addCleanup });
      let configFilePath = path.join(scratchDirectory, "quick-lint-js.config");
      fs.writeFileSync(
        configFilePath,
        '{"globals": {"testGlobalVariable": true}}'
      );
      let configURI = vscode.Uri.file(configFilePath);

      await loadExtensionAsync({ addCleanup });
      let configDocument = await vscode.workspace.openTextDocument(configURI);
      let configEditor = await vscode.window.showTextDocument(configDocument);

      // Wait for possible linting to take effect.
      await sleepAsync(100);

      let configDiags = normalizeDiagnostics(configURI);
      assert.deepStrictEqual(configDiags, []);
    },

  "opening invalid quick-lint-js.config shows diagnostics": async ({
    addCleanup,
  }) => {
    let scratchDirectory = makeScratchDirectory({ addCleanup });
    let configFilePath = path.join(scratchDirectory, "quick-lint-js.config");
    fs.writeFileSync(
      configFilePath,
      '{"globals": {"testGlobalVariable": "INVALID"}}'
    );
    let configURI = vscode.Uri.file(configFilePath);

    await loadExtensionAsync({ addCleanup });
    let configDocument = await vscode.workspace.openTextDocument(configURI);
    let configEditor = await vscode.window.showTextDocument(configDocument);

    await waitUntilAnyDiagnosticsAsync(configURI);

    let configDiags = normalizeDiagnostics(configURI);
    assert.deepStrictEqual(
      configDiags.map(({ code }) => code),
      ["E171"]
    );
  },

  "making quick-lint-js.config invalid shows diagnostics": async ({
    addCleanup,
  }) => {
    let scratchDirectory = makeScratchDirectory({ addCleanup });
    let configFilePath = path.join(scratchDirectory, "quick-lint-js.config");
    fs.writeFileSync(
      configFilePath,
      '{"globals": {"testGlobalVariable": true}}'
    );
    let configURI = vscode.Uri.file(configFilePath);

    await loadExtensionAsync({ addCleanup });
    let configDocument = await vscode.workspace.openTextDocument(configURI);
    let configEditor = await vscode.window.showTextDocument(configDocument);
    await configEditor.edit((editBuilder) => {
      editBuilder.replace(
        new vscode.Range(
          new vscode.Position(0, '{"globals": {"testGlobalVariable": '.length),
          new vscode.Position(
            0,
            '{"globals": {"testGlobalVariable": true'.length
          )
        ),
        '"INVALID"'
      );
    });

    await waitUntilAnyDiagnosticsAsync(configURI);

    let configDiags = normalizeDiagnostics(configURI);
    assert.deepStrictEqual(
      configDiags.map(({ code }) => code),
      ["E171"]
    );
  },

  "opened .js file uses quick-lint-js.config from disk after config editor is closed":
    async ({ addCleanup }) => {
      // TODO(strager): Enable this test when VS Code is fixed (or when we find a
      // workaround). In tests, VS Code does not send us didCloseTextDocument
      // notifications, so this test can't know when quick-lint-js.config is
      // closed.
      // https://github.com/microsoft/vscode/issues/130957
      return;

      let scratchDirectory = makeScratchDirectory({ addCleanup });
      let jsFilePath = path.join(scratchDirectory, "hello.js");
      fs.writeFileSync(
        jsFilePath,
        "testGlobalVariableFromEditor;\ntestGlobalVariableFromDiskModified;"
      );
      let jsURI = vscode.Uri.file(jsFilePath);
      let configFilePath = path.join(scratchDirectory, "quick-lint-js.config");
      fs.writeFileSync(
        configFilePath,
        '{"globals": {"testGlobalVariableFromDiskOriginal": true}}'
      );
      let configURI = vscode.Uri.file(configFilePath);

      await loadExtensionAsync({ addCleanup });
      let configDocument = await vscode.workspace.openTextDocument(configURI);
      let configEditor = await vscode.window.showTextDocument(configDocument);
      await configEditor.edit((editBuilder) => {
        editBuilder.replace(
          new vscode.Range(
            new vscode.Position(0, 0),
            new vscode.Position(1, 0)
          ),
          '{"globals": {"testGlobalVariableFromEditor": true}, "global-groups": ["ecmascript"]}'
        );
      });
      await vscode.commands.executeCommand(
        "workbench.action.closeActiveEditor"
      );
      fs.writeFileSync(
        configFilePath,
        '{"globals": {"testGlobalVariableFromDiskModified": true}}'
      );

      let jsDocument = await vscode.workspace.openTextDocument(jsURI);
      let jsEditor = await vscode.window.showTextDocument(jsDocument);
      await waitUntilAnyDiagnosticsAsync(jsURI);

      let jsDiags = normalizeDiagnostics(jsURI);
      assert.deepStrictEqual(
        jsDiags.map(({ code, startLine }) => ({ code, startLine })),
        [
          {
            code: "E057",
            startLine: 0, // testGlobalVariableFromEditor
          },
        ]
      );
    },

  "opened .js re-lints when changing open quick-lint-js.config": async ({
    addCleanup,
  }) => {
    let scratchDirectory = makeScratchDirectory({ addCleanup });
    let jsFilePath = path.join(scratchDirectory, "hello.js");
    fs.writeFileSync(jsFilePath, "testGlobalVariableFromEditor;");
    let jsURI = vscode.Uri.file(jsFilePath);
    let configFilePath = path.join(scratchDirectory, "quick-lint-js.config");
    fs.writeFileSync(configFilePath, "{}");
    let configURI = vscode.Uri.file(configFilePath);

    await loadExtensionAsync({ addCleanup });
    let jsDocument = await vscode.workspace.openTextDocument(jsURI);
    let jsEditor = await vscode.window.showTextDocument(
      jsDocument,
      vscode.ViewColumn.One
    );
    await waitUntilAnyDiagnosticsAsync(jsURI);

    let configDocument = await vscode.workspace.openTextDocument(configURI);
    let configEditor = await vscode.window.showTextDocument(
      configDocument,
      vscode.ViewColumn.Two
    );
    await configEditor.edit((editBuilder) => {
      editBuilder.replace(
        new vscode.Range(new vscode.Position(0, 0), new vscode.Position(1, 0)),
        '{"globals": {"testGlobalVariableFromEditor": true}}'
      );
    });
    await waitUntilNoDiagnosticsAsync(jsURI);
  },

  "opened .js re-lints when changing unopened quick-lint-js.config on disk":
    async ({ addCleanup }) => {
      let scratchDirectory = makeScratchDirectory({ addCleanup });
      let jsFilePath = path.join(scratchDirectory, "hello.js");
      fs.writeFileSync(jsFilePath, "testGlobalVariableFromDisk;");
      let jsURI = vscode.Uri.file(jsFilePath);
      let configFilePath = path.join(scratchDirectory, "quick-lint-js.config");
      fs.writeFileSync(configFilePath, "{}");
      let configURI = vscode.Uri.file(configFilePath);

      await loadExtensionAsync({ addCleanup });
      let jsDocument = await vscode.workspace.openTextDocument(jsURI);
      let jsEditor = await vscode.window.showTextDocument(
        jsDocument,
        vscode.ViewColumn.One
      );
      await waitUntilAnyDiagnosticsAsync(jsURI);

      fs.writeFileSync(
        configFilePath,
        '{"globals": {"testGlobalVariableFromDisk": true}}'
      );
      await waitUntilNoDiagnosticsAsync(jsURI);
    },

  "no output channel by default": async ({ addCleanup }) => {
    let outputChannelMocker = VSCodeOutputChannelMocker.mock({ addCleanup });

    let scratchDirectory = makeScratchDirectory({ addCleanup });
    let jsFilePath = path.join(scratchDirectory, "hello.js");
    fs.writeFileSync(jsFilePath, "test;");
    let jsURI = vscode.Uri.file(jsFilePath);

    await loadExtensionAsync({ addCleanup });
    let jsDocument = await vscode.workspace.openTextDocument(jsURI);
    let jsEditor = await vscode.window.showTextDocument(
      jsDocument,
      vscode.ViewColumn.One
    );

    assert.deepStrictEqual(outputChannelMocker.getOutputChannels(), []);
  },

  "output channel gets messages if logging is enabled": async ({
    addCleanup,
  }) => {
    let outputChannelMocker = VSCodeOutputChannelMocker.mock({ addCleanup });

    await vscode.workspace
      .getConfiguration("quick-lint-js")
      .update("logging", "verbose", vscode.ConfigurationTarget.Workspace);
    addCleanup(resetConfigurationAsync);

    let scratchDirectory = makeScratchDirectory({ addCleanup });
    let jsFilePath = path.join(scratchDirectory, "hello.js");
    fs.writeFileSync(jsFilePath, "test;");
    let jsURI = vscode.Uri.file(jsFilePath);

    await loadExtensionAsync({ addCleanup });
    let jsDocument = await vscode.workspace.openTextDocument(jsURI);
    let jsEditor = await vscode.window.showTextDocument(
      jsDocument,
      vscode.ViewColumn.One
    );

    assert.deepStrictEqual(
      outputChannelMocker.getOutputChannels().map((c) => c.name),
      ["quick-lint-js"]
    );
    let channel = outputChannelMocker.getOutputChannels()[0];
    assert.ok(
      channel._data.length > 0,
      "at least one message should have been logged by opening the file"
    );
  },

  // TODO(strager): Allow user to turn logging on or off after loading the
  // extension.

  // TODO(strager): Allow the user to delete the extenion, thereby deleting
  // the output channel.
};

if (os.platform() === "linux") {
  tests = {
    ...tests,
    "Linux: inotify watch error shows pop-up": async ({ addCleanup }) => {
      let messageMocker = VSCodeMessageMocker.mock({ addCleanup });
      mockInotifyErrors({
        addCleanup,
        addWatchError: os.constants.errno.ENOSPC,
      });

      let scratchDirectory = makeScratchDirectory({ addCleanup });
      let jsFilePath = path.join(scratchDirectory, "hello.js");
      fs.writeFileSync(jsFilePath, "");
      let jsURI = vscode.Uri.file(jsFilePath);

      await loadExtensionAsync({ addCleanup });
      let jsDocument = await vscode.workspace.openTextDocument(jsURI);
      let jsEditor = await vscode.window.showTextDocument(jsDocument);

      await messageMocker.waitUntilAnyMessageAsync();
      messageMocker.assertAnyWarningMessageMatches(
        /failed to watch .* for changes/i
      );
    },

    "Linux: inotify init error shows pop-up": async ({ addCleanup }) => {
      let messageMocker = VSCodeMessageMocker.mock({ addCleanup });
      mockInotifyErrors({ addCleanup, initError: os.constants.errno.EMFILE });

      let scratchDirectory = makeScratchDirectory({ addCleanup });
      let jsFilePath = path.join(scratchDirectory, "hello.js");
      fs.writeFileSync(jsFilePath, "");
      let jsURI = vscode.Uri.file(jsFilePath);

      await loadExtensionAsync({ addCleanup });
      let jsDocument = await vscode.workspace.openTextDocument(jsURI);
      let jsEditor = await vscode.window.showTextDocument(jsDocument);

      await messageMocker.waitUntilAnyMessageAsync();
      // TODO(strager): Improve the message.
      messageMocker.assertAnyWarningMessageMatches(
        /failed to watch  for changes/i
      );
    },
  };
}

if (os.platform() === "darwin") {
  tests = {
    ...tests,
    "BSD: directory watch error shows pop-up": async ({ addCleanup }) => {
      let messageMocker = VSCodeMessageMocker.mock({ addCleanup });
      mockKqueueErrors({
        addCleanup,
        directoryOpenError: os.constants.errno.EMFILE,
      });

      let scratchDirectory = makeScratchDirectory({ addCleanup });
      let jsFilePath = path.join(scratchDirectory, "hello.js");
      fs.writeFileSync(jsFilePath, "");
      let jsURI = vscode.Uri.file(jsFilePath);

      await loadExtensionAsync({ addCleanup });
      let jsDocument = await vscode.workspace.openTextDocument(jsURI);
      let jsEditor = await vscode.window.showTextDocument(jsDocument);

      await messageMocker.waitUntilAnyMessageAsync();
      messageMocker.assertAnyWarningMessageMatches(
        /failed to watch .* for changes/i
      );
    },
  };
}

if (["darwin", "linux"].includes(os.platform())) {
  tests = {
    ...tests,
    "watch error only shows pop-up once": async ({ addCleanup }) => {
      let messageMocker = VSCodeMessageMocker.mock({ addCleanup });
      switch (os.platform()) {
        case "darwin":
          mockKqueueErrors({
            addCleanup,
            directoryOpenError: os.constants.errno.EMFILE,
          });
          break;
        case "linux":
          mockInotifyErrors({
            addCleanup,
            addWatchError: os.constants.errno.ENOSPC,
          });
          break;
      }

      let scratchDirectory = makeScratchDirectory({ addCleanup });
      let jsFilePath = path.join(scratchDirectory, "hello.js");
      fs.writeFileSync(jsFilePath, "");
      let jsURI = vscode.Uri.file(jsFilePath);

      await loadExtensionAsync({ addCleanup });
      let jsDocument = await vscode.workspace.openTextDocument(jsURI);
      await vscode.window.showTextDocument(jsDocument);

      await messageMocker.waitUntilAnyMessageAsync();
      assert.strictEqual(
        messageMocker.getWarningMessages().length,
        1,
        `Expected exactly one warning message. Got messages: ${messageMocker.getMessagesDebugString()}`
      );
      messageMocker.clearRememberedMessages();

      fs.mkdirSync(path.join(scratchDirectory, "dir"));
      let otherJSFilePath = path.join(scratchDirectory, "other.js");
      fs.writeFileSync(otherJSFilePath, "SYNTAX ERROR");
      let otherJSURI = vscode.Uri.file(otherJSFilePath);
      let otherJSDocument = await vscode.workspace.openTextDocument(otherJSURI);
      await vscode.window.showTextDocument(otherJSDocument);
      await waitUntilAnyDiagnosticsAsync(otherJSURI);

      assert.deepStrictEqual(
        messageMocker.getWarningMessages(),
        [],
        `Expected no more warning messages. Got messages: ${messageMocker.getMessagesDebugString()}`
      );
    },
  };
}

for (let testName in tests) {
  let realTestFunction = tests[testName];
  tests[testName] = (fixture) => {
    fixture.addCleanup(async () => {
      await cleanUpVSCodeEditorsAsync();
    });
    return realTestFunction(fixture);
  };
}

async function cleanUpVSCodeEditorsAsync() {
  await vscode.commands.executeCommand("workbench.action.closeAllEditors");
}

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

class VSCodeMessageMocker {
  static mock({ addCleanup }) {
    let mocker = new VSCodeMessageMocker();

    let methodsToMock = [
      "showErrorMessage",
      "showInformationMessage",
      "showWarningMessage",
    ];
    for (let methodToMock of methodsToMock) {
      mocker._mockMethod(methodToMock, { addCleanup });
    }

    return mocker;
  }

  constructor() {
    this._messages = [];
  }

  getMessagesDebugString() {
    return JSON.stringify(this._messages);
  }

  getErrorMessages() {
    return this._messages.filter((m) => m.method === "showErrorMessage");
  }

  getWarningMessages() {
    return this._messages.filter((m) => m.method === "showWarningMessage");
  }

  clearRememberedMessages() {
    this._messages.length = 0;
  }

  assertNoMessages() {
    assert.deepStrictEqual(this._messages, []);
  }

  assertAnyErrorMessageMatches(regExp) {
    assert.strictEqual(
      this.getErrorMessages().some((m) => regExp.test(m.message)),
      true,
      `Expected message indicating IO error. Got messages: ${this.getMessagesDebugString()}`
    );
  }

  assertAnyWarningMessageMatches(regExp) {
    assert.strictEqual(
      this.getWarningMessages().some((m) => regExp.test(m.message)),
      true,
      `Expected warning message. Got messages: ${this.getMessagesDebugString()}`
    );
  }

  async waitUntilAnyMessageAsync() {
    await pollAsync(async () => {
      assert.ok(this._messages.length >= 1);
    });
  }

  _mockMethod(methodToMock, { addCleanup }) {
    let originalMethod = vscode.window[methodToMock];
    addCleanup(() => {
      vscode.window[methodToMock] = originalMethod;
    });

    let self = this;
    vscode.window[methodToMock] = function showMessageMock(message, ...args) {
      console.log(
        `called: vscode.window.${methodToMock}(${JSON.stringify(message)}, ...)`
      );
      let buttons = args;
      return new Promise((resolve, _reject) => {
        self._messages.push({
          message: message,
          method: methodToMock,
          clickButton(buttonLabel) {
            assert.ok(
              buttons.includes(buttonLabel),
              `Cannot click button ${JSON.stringify(
                buttonLabel
              )}; available buttons are: ${JSON.stringify(buttons)}`
            );
            resolve(buttonLabel);
          },
          dismiss() {
            resolve(undefined);
          },
        });
      });
    };
  }
}

class VSCodeOutputChannelMocker {
  static mock({ addCleanup }) {
    let mocker = new VSCodeOutputChannelMocker();

    let originalCreateOutputChannel = vscode.window.createOutputChannel;
    addCleanup(() => {
      vscode.window.createOutputChannel = originalCreateOutputChannel;
    });
    vscode.window.createOutputChannel = (name) => {
      return mocker._createOutputChannel(name);
    };

    return mocker;
  }

  constructor() {
    this._outputChannels = [];
  }

  getOutputChannels() {
    return [...this._outputChannels];
  }

  _createOutputChannel(name) {
    console.log(
      `called: vscode.window.createOutputChannel(${JSON.stringify(name)})`
    );
    let channel = new FakeOutputChannel(name);
    this._outputChannels.push(channel);
    return channel;
  }
}

class FakeOutputChannel /*:: implements vscode.OutputChannel */ {
  constructor(name) {
    this._name = name;
    this._data = "";
  }

  get name() {
    return this._name;
  }

  append(value) {
    this._data += value;
  }

  appendLine(value) {
    this.append(value);
    this.append("\n");
  }

  clear() {
    this._data = "";
  }

  show() {
    // Ignore.
  }

  hide() {
    // Ignore.
  }

  dispose() {}
}

function mockInotifyErrors({ addCleanup, addWatchError = 0, initError = 0 }) {
  qljsExtension.mockInotifyErrors(initError, addWatchError);
  addCleanup(() => {
    qljsExtension.mockInotifyErrors(0, 0);
  });
}

function mockKqueueErrors({ addCleanup, directoryOpenError = 0 }) {
  qljsExtension.mockKqueueErrors(directoryOpenError);
  addCleanup(() => {
    qljsExtension.mockKqueueErrors(0);
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
  // TODO(strager): Don't call realpath. realpath is currently needed to work
  // around bugs in quick-lint-js regarding symlinks.
  return fs.realpathSync(scratchDirectory);
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

async function resetConfigurationAsync() {
  await vscode.workspace
    .getConfiguration("quick-lint-js")
    .update("logging", undefined, vscode.ConfigurationTarget.Workspace);
}

async function runAsync() {
  // vscode-test activated the extension for us. We want tests to be activate and
  // deactivate the extension at will.
  let extension = vscode.extensions.getExtension("quick-lint.quick-lint-js");
  await extension.activate();
  await qljsExtension.deactivate();

  // Disable features of extensions which might conflict with quick-lint-js.
  // NOTE(strager): We can't completely disable the vscode.javascript extension,
  // because without its JavaScript file detection, our extension doesn't work.
  let vscodeConfig = vscode.workspace.getConfiguration();
  await vscodeConfig.update("javascript.validate.enable", false);

  // Clean up configuration in case a previous run didn't clean it up.
  await resetConfigurationAsync();

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
