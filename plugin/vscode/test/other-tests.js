// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

"use strict";

let path = require("path");
process.env.NODE_PATH =
  path.resolve(__dirname, "mock") + path.delimiter + process.env.NODE_PATH;
require("module").Module._initPaths();

let assert = require("assert");
let extension = require("../extension.js");
let qljs = require("../quick-lint-js.js");
let vscode = require("./mock/vscode.js");
let { testMainAsync } = require("./test-support.js");

class MockDocument {
  constructor(uriString, text) {
    this._uriString = uriString;
    this._text = text;
  }

  get uri() {
    return this._uriString;
  }

  getText() {
    return this._text;
  }
}

class FakeDiagnosticCollection {
  constructor() {
    this._diagnostics = new Map();
  }

  set(uri, diagnostics) {
    this._diagnostics.set(uri.toString(), [...diagnostics]);
  }

  get(uri) {
    return this._diagnostics.get(uri.toString());
  }

  delete(uri) {
    this._diagnostics.delete(uri.toString());
  }
}

let tests = {
  "diagnostic severity": async ({ addCleanup }) => {
    let diagnosticCollection = new FakeDiagnosticCollection();
    let linters = new extension.DocumentLinterCollection(diagnosticCollection);
    addCleanup(async () => {
      await linters.disposeAsync();
    });

    let document = new MockDocument(
      "hello.js",
      "let x;let x;\nundeclaredVariable"
    );
    let linter = linters.getLinter(document);
    await linter.editorChangedVisibilityAsync();

    let diags = normalizeDiagnostics(diagnosticCollection.get(document.uri));
    assert.deepStrictEqual(
      diags.map((diag) => ({ message: diag.message, severity: diag.severity })),
      [
        {
          message: "redeclaration of variable: x",
          severity: vscode.DiagnosticSeverity.Error,
        },
        {
          message: "use of undeclared variable: undeclaredVariable",
          severity: vscode.DiagnosticSeverity.Warning,
        },
      ]
    );
  },

  "opening editor lints": async ({ addCleanup }) => {
    let diagnosticCollection = new FakeDiagnosticCollection();
    let linters = new extension.DocumentLinterCollection(diagnosticCollection);
    addCleanup(async () => {
      await linters.disposeAsync();
    });

    let document = new MockDocument("hello.js", "let x;let x;");
    let linter = linters.getLinter(document);
    await linter.editorChangedVisibilityAsync();

    let diags = normalizeDiagnostics(diagnosticCollection.get(document.uri));
    assert.deepStrictEqual(
      diags.map((diag) => diag.message),
      ["redeclaration of variable: x"]
    );
  },

  "applying change to unopened editor lints": async ({ addCleanup }) => {
    let diagnosticCollection = new FakeDiagnosticCollection();
    let linters = new extension.DocumentLinterCollection(diagnosticCollection);
    addCleanup(async () => {
      await linters.disposeAsync();
    });

    let document = new MockDocument("hello.js", "let x;");
    let linter = linters.getLinter(document);
    assert.deepStrictEqual(diagnosticCollection.get(document.uri) || [], []);

    let changes = [
      {
        range: new vscode.Range(
          new vscode.Position(0, document._text.length),
          new vscode.Position(0, document._text.length)
        ),
        rangeOffset: 0,
        rangeLength: 0,
        text: "let x;",
      },
    ];
    document._text += changes[0].text;
    await linter.textChangedAsync(changes);

    let diags = normalizeDiagnostics(diagnosticCollection.get(document.uri));
    assert.deepStrictEqual(
      diags.map((diag) => diag.message),
      ["redeclaration of variable: x"]
    );
  },

  "applying multiple changes ignores document text": async ({ addCleanup }) => {
    // NOTE(strager): This test is testing an implementation detail. Parsing
    // based on changes should be preferred; document.getText() should only be
    // called as a last resort.

    let diagnosticCollection = new FakeDiagnosticCollection();
    let linters = new extension.DocumentLinterCollection(diagnosticCollection);
    addCleanup(async () => {
      await linters.disposeAsync();
    });

    let document = new MockDocument("hello.js", "let x;");
    let linter = linters.getLinter(document);
    await linter.editorChangedVisibilityAsync();
    assert.deepStrictEqual(diagnosticCollection.get(document.uri) || [], []);

    let effectiveDocumentText = document._text;
    for (let charactersToType of ["let", " x;", " // done"]) {
      let changes = [
        {
          range: new vscode.Range(
            new vscode.Position(0, effectiveDocumentText.length),
            new vscode.Position(0, effectiveDocumentText.length)
          ),
          rangeOffset: 0,
          rangeLength: 0,
          text: charactersToType,
        },
      ];
      // N.B. Do not update document._text. linter should not inspect
      // document._text; it should only use the given changes.
      effectiveDocumentText += charactersToType;
      await linter.textChangedAsync(changes);
    }

    let diags = normalizeDiagnostics(diagnosticCollection.get(document.uri));
    assert.deepStrictEqual(
      diags.map((diag) => diag.message),
      ["redeclaration of variable: x"]
    );
  },

  "dispose unused linter": async ({ addCleanup }) => {
    let diagnosticCollection = new FakeDiagnosticCollection();
    let linters = new extension.DocumentLinterCollection(diagnosticCollection);
    addCleanup(async () => {
      await linters.disposeAsync();
    });

    let document = new MockDocument("hello.js", "let x;");
    let linter = linters.getLinter(document);
    // Should not throw.
    await linter.disposeAsync();
  },

  "dispose initializing linter": async ({ addCleanup }) => {
    let diagnosticCollection = new FakeDiagnosticCollection();
    let linters = new extension.DocumentLinterCollection(diagnosticCollection);
    addCleanup(async () => {
      await linters.disposeAsync();
    });

    let document = new MockDocument("hello.js", "let x;");
    let linter = linters.getLinter(document);
    let promise = linter.editorChangedVisibilityAsync();

    // Should not throw.
    await linter.disposeAsync();
    await assert.rejects(promise, extension.DocumentLinterDisposed);
  },

  "concurrent edits are applied in order of calls": async ({ addCleanup }) => {
    let diagnosticCollection = new FakeDiagnosticCollection();
    let linters = new extension.DocumentLinterCollection(diagnosticCollection);
    addCleanup(async () => {
      await linters.disposeAsync();
    });

    let document = new MockDocument("hello.js", "let x;");
    let linter = linters.getLinter(document);
    await linter.editorChangedVisibilityAsync();

    let promises = [];
    for (let characterToType of "let x; // done") {
      let changes = [
        {
          range: new vscode.Range(
            new vscode.Position(0, document._text.length),
            new vscode.Position(0, document._text.length)
          ),
          rangeOffset: 0,
          rangeLength: 0,
          text: characterToType,
        },
      ];
      document._text += characterToType;
      promises.push(linter.textChangedAsync(changes));
    }
    await Promise.all(promises);

    let diags = normalizeDiagnostics(diagnosticCollection.get(document.uri));
    assert.deepStrictEqual(
      diags.map((diag) => ({
        start: diag.startCharacter,
        end: diag.endCharacter,
      })),
      [{ start: "let x;let ".length, end: "let x;let x".length }]
    );
  },

  "open editor and make changes, with exhaustive fault injection": async ({
    addCleanup,
  }) => {
    let xRedeclarationDiagnostic = {
      message: "redeclaration of variable: x",
      severity: vscode.DiagnosticSeverity.Error,
      source: "quick-lint-js",
      startLine: 0,
      startCharacter: "let x;let ".length,
      endLine: 0,
      endCharacter: "let x;let x".length,
    };
    let yRedeclarationDiagnostic = {
      message: "redeclaration of variable: y",
      severity: vscode.DiagnosticSeverity.Error,
      source: "quick-lint-js",
      startLine: 1,
      startCharacter: "let y;let ".length,
      endLine: 1,
      endCharacter: "let y;let y".length,
    };

    let coinFlips;
    let rng = new ExhaustiveRNG();
    injectFaults(
      (functionName) => {
        // TODO(strager): Figure out why qljs_vscode_create_parser failures
        // cause this test to fail.
        if (functionName !== "qljs_vscode_create_parser") {
          let shouldCrash = rng.nextCoinFlip();
          coinFlips.push(shouldCrash);
          if (shouldCrash) {
            throw new qljs.ProcessCrashed("(injected fault)");
          }
        }
      },
      { addCleanup }
    );

    while (!rng.isDone()) {
      coinFlips = [];
      let diagnosticCollection = new FakeDiagnosticCollection();
      let linters = new extension.DocumentLinterCollection(
        diagnosticCollection
      );
      try {
        let document = new MockDocument("hello.js", "let x;let x;\n");
        let linter = linters.getLinter(document);

        let crashedOpeningEditor = await didLintingCrashAsync(async () => {
          await linter.editorChangedVisibilityAsync();
        });
        if (crashedOpeningEditor) {
          // Linter crashed before any linting could have happened.
          // Therefore, no diagnostics should appear.
          let diags = normalizeDiagnostics(
            diagnosticCollection.get(document.uri)
          );
          assert.deepStrictEqual(diags, []);
        } else {
          let crashedChangingText = await didLintingCrashAsync(async () => {
            document._text = "let x;let x;\nlet y;let y;";
            await linter.textChangedAsync([
              {
                range: new vscode.Range(
                  new vscode.Position(1, 0),
                  new vscode.Position(1, 0)
                ),
                rangeOffset: 0,
                rangeLength: 0,
                text: "let y;let y;",
              },
            ]);
          });
          let diags = normalizeDiagnostics(
            diagnosticCollection.get(document.uri)
          );
          if (crashedChangingText) {
            // Linter crashed after linting happened, but before linting the
            // changes could have happened. Therefore, diagnostics should
            // appear for the old version of the document.
            assert.deepStrictEqual(diags, [xRedeclarationDiagnostic]);
          } else {
            // Crashes might have happened, but DocumentLinter should have
            // recovered.
            assert.deepStrictEqual(diags, [
              xRedeclarationDiagnostic,
              yRedeclarationDiagnostic,
            ]);
          }
        }
      } finally {
        await linters.disposeAsync();
      }

      console.log(`coinFlips: ${coinFlips}`);
      rng.lap();
    }

    async function didLintingCrashAsync(callback) {
      try {
        await callback();
        return false;
      } catch (e) {
        if (e instanceof extension.LintingCrashed) {
          return true;
        } else {
          throw e;
        }
      }
    }
  },

  "concurrent edits are applied in order of calls, with exhaustive fault injection": async ({
    addCleanup,
  }) => {
    let coinFlips;
    let rng = new ExhaustiveRNG();
    function maybeInjectFaultWithExhaustiveRNG() {
      let shouldCrash = rng.nextCoinFlip();
      coinFlips.push(shouldCrash);
      if (shouldCrash) {
        throw new qljs.ProcessCrashed("(injected fault)");
      }
    }

    let oldMaybeInjectFault = qljs.maybeInjectFault;
    addCleanup(() => {
      qljs.maybeInjectFault = oldMaybeInjectFault;
    });

    while (!rng.isDone()) {
      coinFlips = [];
      let diagnosticCollection = new FakeDiagnosticCollection();
      let linters = new extension.DocumentLinterCollection(
        diagnosticCollection
      );

      try {
        let document = new MockDocument("hello.js", "let x;");
        let linter = linters.getLinter(document);
        let shouldOpenEditorBeforeChanges = rng.nextCoinFlip();
        if (shouldOpenEditorBeforeChanges) {
          await linter.editorChangedVisibilityAsync();
        }

        qljs.maybeInjectFault = maybeInjectFaultWithExhaustiveRNG;
        let promises = [];
        for (let charactersToType of ["let ", "x;"]) {
          let changes = [
            {
              range: new vscode.Range(
                new vscode.Position(0, document._text.length),
                new vscode.Position(0, document._text.length)
              ),
              rangeOffset: 0,
              rangeLength: 0,
              text: charactersToType,
            },
          ];
          document._text += charactersToType;
          promises.push(linter.textChangedAsync(changes));
        }

        let textChangedResults = await Promise.allSettled(promises);
        let firstChangeFailed = textChangedResults[0].status !== "fulfilled";
        let lastChangeFailed = textChangedResults[1].status !== "fulfilled";

        let diags = normalizeDiagnostics(
          diagnosticCollection.get(document.uri)
        );
        if (firstChangeFailed && lastChangeFailed) {
          // No changes were applied. The linted document was "let x;".
          assert.deepStrictEqual(
            diags.map((diag) => diag.message),
            []
          );
        } else if (!firstChangeFailed && lastChangeFailed) {
          // Partial changes were applied. The linted document was either
          // "let x;let " (if the first change finished before the second change
          // started) or "let x; let x;" (if the second change failed before the
          // first change started).
          let messages = diags.map((diag) => diag.message);
          assert.strictEqual(messages.length, 1, messages);
          assert.ok(
            messages[0] === "let with no bindings" ||
              messages[0] === "redeclaration of variable: x",
            messages
          );
        } else {
          // Because the last call to textChangedAsync succeeded, all changes
          // were applied. The linted document was "let x;let x;".
          assert.deepStrictEqual(
            diags.map((diag) => diag.message),
            ["redeclaration of variable: x"]
          );
        }
      } finally {
        await linters.disposeAsync();
      }

      console.log(`coinFlips: ${coinFlips}`);
      rng.lap();
      qljs.maybeInjectFault = oldMaybeInjectFault;
    }
  },

  "ExhaustiveRNG: exhaust with no calls": () => {
    let rng = new ExhaustiveRNG();
    rng.lap();
    assert.ok(rng.isDone());
  },

  "ExhaustiveRNG: coin flip has two laps": () => {
    let rng = new ExhaustiveRNG();
    rng.nextCoinFlip();
    rng.lap();
    assert.ok(!rng.isDone());
    rng.nextCoinFlip();
    rng.lap();
    assert.ok(rng.isDone());
  },

  "ExhaustiveRNG: coin flip returns false then true": () => {
    let rng = new ExhaustiveRNG();
    assert.strictEqual(
      rng.nextCoinFlip(),
      false,
      "first coin flip should be false"
    );
    rng.lap();
    assert.strictEqual(
      rng.nextCoinFlip(),
      true,
      "second coin flip should be true"
    );
  },

  "ExhaustiveRNG: coin flip and maybe another coin flip (if true) per lap": () => {
    let rng = new ExhaustiveRNG();

    assert.strictEqual(rng.nextCoinFlip(), false);
    rng.lap();

    assert.strictEqual(rng.nextCoinFlip(), true);
    assert.strictEqual(rng.nextCoinFlip(), false);
    rng.lap();

    assert.strictEqual(rng.nextCoinFlip(), true);
    assert.strictEqual(rng.nextCoinFlip(), true);
    rng.lap();

    assert.ok(rng.isDone());
  },

  "ExhaustiveRNG: coin flip and maybe another coin flip (if false) per lap": () => {
    let rng = new ExhaustiveRNG();

    assert.strictEqual(rng.nextCoinFlip(), false);
    assert.strictEqual(rng.nextCoinFlip(), false);
    rng.lap();

    assert.strictEqual(rng.nextCoinFlip(), false);
    assert.strictEqual(rng.nextCoinFlip(), true);
    rng.lap();

    assert.strictEqual(rng.nextCoinFlip(), true);
    rng.lap();

    assert.ok(rng.isDone());
  },

  "ExhaustiveRNG: exhaust with three nextCoinFlip calls per lap": () => {
    let expectedOutcomesPerLap = [
      [false, false, false],
      [false, false, true],
      [false, true, false],
      [false, true, true],
      [true, false, false],
      [true, false, true],
      [true, true, false],
      [true, true, true],
    ];
    let expectedLaps = expectedOutcomesPerLap.length;
    let rng = new ExhaustiveRNG();
    let i = 0;
    while (i < expectedLaps) {
      let actual0 = rng.nextCoinFlip();
      let actual1 = rng.nextCoinFlip();
      let actual2 = rng.nextCoinFlip();
      let iString = i.toString();
      let expected = expectedOutcomesPerLap[i];
      assert.deepStrictEqual(
        [actual0, actual1, actual2],
        expected,
        `nextCoinFlip-s of lap ${iString}`
      );
      rng.lap();
      i += 1;
      if (rng.isDone()) {
        break;
      }
    }
    assert.strictEqual(i, expectedLaps, "number of laps");
    assert.ok(
      rng.isDone(),
      `rng should be done after ${expectedLaps.toString()} laps`
    );
  },
};

function injectFaults(faultCallback, { addCleanup }) {
  let oldMaybeInjectFault = qljs.maybeInjectFault;
  qljs.maybeInjectFault = faultCallback;
  addCleanup(() => {
    qljs.maybeInjectFault = oldMaybeInjectFault;
  });
}

// Convert an array of vscode.Diagnostic into an array of plain JavaScript
// objects. Use this with assert.deepStrictEqual in tests.
function normalizeDiagnostics(vscodeDiagnostics) {
  vscodeDiagnostics = vscodeDiagnostics || [];
  return vscodeDiagnostics.map((diag) => ({
    message: diag.message,
    source: diag.source,
    severity: diag.severity,
    startLine: diag.range.start.line,
    startCharacter: diag.range.start.character,
    endLine: diag.range.end.line,
    endCharacter: diag.range.end.character,
  }));
}

class ExhaustiveRNG {
  constructor() {
    this._counterIndex = 0;
    this._counters = [];
    this._done = false;
  }

  // Returns true or false.
  nextCoinFlip() {
    if (this._counterIndex >= this._counters.length) {
      this._counters.push(false);
    }
    let result = this._counters[this._counterIndex];
    this._counterIndex += 1;
    return result;
  }

  isDone() {
    return this._done;
  }

  lap() {
    // Update this._counters from right to left.
    let i = this._counterIndex;
    for (;;) {
      if (i === 0) {
        this._done = true;
        break;
      }
      i -= 1;
      if (this._counters[i]) {
        this._counters[i] = false; // Clear for next lap.
      } else {
        this._counters[i] = true;
        break;
      }
    }

    this._counterIndex = 0;
  }
}

async function mainAsync() {
  await testMainAsync(tests);
}
exports.mainAsync = mainAsync;

if (require.main === module) {
  mainAsync();
}
