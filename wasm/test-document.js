// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

"use strict";

let assert = require("assert");
let {
  DocumentLinter,
  createProcessFactoryAsync,
} = require("./quick-lint-js.js");

let processFactoryPromise = createProcessFactoryAsync();

describe("DocumentLinter", () => {
  let toDisposeAfterTest = [];
  function disposeAfterTest(disposable) {
    toDisposeAfterTest.push(disposable);
    return disposable;
  }
  afterEach(async () => {
    for (let toDispose of toDisposeAfterTest) {
      await toDispose.disposeAsync();
    }
  });

  it("opening editor lints", async () => {
    let document = new MockDocument("let x;let x;");
    let linter = disposeAfterTest(
      new DocumentLinter(document, processFactoryPromise)
    );

    await linter.editorChangedVisibilityAsync();

    assert.deepStrictEqual(document.getDiagnosticMessages(), [
      "redeclaration of variable: x",
    ]);
  });
});

class MockDocument {
  constructor(text) {
    this.text = text;
    this.diagnostics = [];
  }

  getText() {
    return this.text;
  }

  setDiagnostics(diagnostics) {
    this.diagnostics = diagnostics;
  }

  removeDiagnostics() {
    this.diagnostics = [];
  }

  getDiagnosticMessages() {
    return this.diagnostics.map((diag) => diag.message);
  }
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
