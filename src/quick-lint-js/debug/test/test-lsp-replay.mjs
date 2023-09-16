// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import assert from "node:assert/strict";
import {
  LSPReplayer,
  indexFromLSPLocation,
  updateLSPDocumentText,
} from "../public/lsp-replay.mjs";
import { describe, it } from "node:test";

describe("LSP replay", () => {
  it("initial state has no documents", () => {
    let replayer = new LSPReplayer();
    assert.deepEqual(
      replayer.getOpenedDocumentsBeforeMessageIndex(0),
      []
    );
  });

  it("out of bounds message index throws", () => {
    let replayer = new LSPReplayer();
    assert.throws(() => {
      replayer.getOpenedDocumentsBeforeMessageIndex(-1);
    }, RangeError);
    assert.throws(() => {
      replayer.getOpenedDocumentsBeforeMessageIndex(1);
    }, RangeError);
  });

  it("textDocument/didOpen message adds document", () => {
    let replayer = new LSPReplayer();
    replayer.appendClientToServerMessage({
      method: "textDocument/didOpen",
      params: {
        textDocument: {
          uri: "file:///hello.js",
          version: 2,
          languageId: "javascript",
          text: "hello world",
        },
      },
    });
    assert.deepEqual(replayer.getOpenedDocumentsBeforeMessageIndex(1), [
      {
        uri: "file:///hello.js",
        version: 2,
        languageID: "javascript",
        text: "hello world",
      },
    ]);
  });

  it("textDocument/didChange full-change message replaces opened document's text and version", () => {
    let replayer = new LSPReplayer();
    replayer.appendClientToServerMessage({
      method: "textDocument/didOpen",
      params: {
        textDocument: {
          uri: "file:///hello.js",
          version: 2,
          languageId: "javascript",
          text: "hello world",
        },
      },
    });
    replayer.appendClientToServerMessage({
      method: "textDocument/didChange",
      params: {
        textDocument: {
          uri: "file:///hello.js",
          version: 3,
        },
        contentChanges: [
          {
            text: "updated world",
          },
        ],
      },
    });

    assert.deepEqual(replayer.getOpenedDocumentsBeforeMessageIndex(2), [
      {
        uri: "file:///hello.js",
        version: 3,
        languageID: "javascript",
        text: "updated world",
      },
    ]);

    // Older versions should be correct too:
    assert.deepEqual(
      replayer.getOpenedDocumentsBeforeMessageIndex(0),
      []
    );
    assert.deepEqual(replayer.getOpenedDocumentsBeforeMessageIndex(1), [
      {
        uri: "file:///hello.js",
        version: 2,
        languageID: "javascript",
        text: "hello world",
      },
    ]);
  });

  it("textDocument/didChange partial-change message updates opened document's text and version", () => {
    let replayer = new LSPReplayer();
    replayer.appendClientToServerMessage({
      method: "textDocument/didOpen",
      params: {
        textDocument: {
          uri: "file:///hello.js",
          version: 2,
          languageId: "javascript",
          text: "hello world",
        },
      },
    });
    replayer.appendClientToServerMessage({
      method: "textDocument/didChange",
      params: {
        textDocument: {
          uri: "file:///hello.js",
          version: 3,
        },
        contentChanges: [
          {
            range: {
              start: { line: 0, character: 0 },
              end: { line: 0, character: 5 },
            },
            text: "updated",
          },
        ],
      },
    });

    assert.deepEqual(replayer.getOpenedDocumentsBeforeMessageIndex(2), [
      {
        uri: "file:///hello.js",
        version: 3,
        languageID: "javascript",
        text: "updated world",
      },
    ]);
  });

  it("textDocument/didChange partial-change without open message reports unknown document text", () => {
    let replayer = new LSPReplayer();
    replayer.appendClientToServerMessage({
      method: "textDocument/didChange",
      params: {
        textDocument: {
          uri: "file:///hello.js",
          version: 3,
        },
        contentChanges: [
          {
            range: {
              start: { line: 0, character: 0 },
              end: { line: 0, character: 0 },
            },
            text: "sketchy update",
          },
        ],
      },
    });

    assert.deepEqual(replayer.getOpenedDocumentsBeforeMessageIndex(1), [
      {
        uri: "file:///hello.js",
        version: 3,
        languageID: LSPReplayer.UNKNOWN_DOCUMENT_LANGUAGE_ID,
        text: LSPReplayer.UNKNOWN_DOCUMENT_TEXT,
      },
    ]);
  });

  it("two textDocument/didChange partial-changes without open message reports unknown document text", () => {
    let replayer = new LSPReplayer();
    replayer.appendClientToServerMessage({
      method: "textDocument/didChange",
      params: {
        textDocument: {
          uri: "file:///hello.js",
          version: 3,
        },
        contentChanges: [
          {
            range: {
              start: { line: 0, character: 0 },
              end: { line: 0, character: 0 },
            },
            text: "sketchy update",
          },
        ],
      },
    });
    replayer.appendClientToServerMessage({
      method: "textDocument/didChange",
      params: {
        textDocument: {
          uri: "file:///hello.js",
          version: 4,
        },
        contentChanges: [
          {
            range: {
              start: { line: 0, character: 0 },
              end: { line: 0, character: 0 },
            },
            text: "more updates",
          },
        ],
      },
    });

    assert.deepEqual(replayer.getOpenedDocumentsBeforeMessageIndex(2), [
      {
        uri: "file:///hello.js",
        version: 4,
        languageID: LSPReplayer.UNKNOWN_DOCUMENT_LANGUAGE_ID,
        text: LSPReplayer.UNKNOWN_DOCUMENT_TEXT,
      },
    ]);
  });

  it("textDocument/didChange full-change without open message reports document text and unknown language ID", () => {
    let replayer = new LSPReplayer();
    replayer.appendClientToServerMessage({
      method: "textDocument/didChange",
      params: {
        textDocument: {
          uri: "file:///hello.js",
          version: 3,
        },
        contentChanges: [
          {
            text: "sketchy update",
          },
        ],
      },
    });

    assert.deepEqual(replayer.getOpenedDocumentsBeforeMessageIndex(1), [
      {
        uri: "file:///hello.js",
        version: 3,
        languageID: LSPReplayer.UNKNOWN_DOCUMENT_LANGUAGE_ID,
        text: "sketchy update",
      },
    ]);
  });

  it("textDocument/didClose makes document disappear", () => {
    let replayer = new LSPReplayer();
    replayer.appendClientToServerMessage({
      method: "textDocument/didOpen",
      params: {
        textDocument: {
          uri: "file:///hello.js",
          version: 2,
          languageId: "javascript",
          text: "hello world",
        },
      },
    });
    replayer.appendClientToServerMessage({
      method: "textDocument/didClose",
      params: {
        textDocument: {
          uri: "file:///hello.js",
        },
      },
    });

    assert.deepEqual(
      replayer.getOpenedDocumentsBeforeMessageIndex(2),
      []
    );
  });
});

// See also: test/test-lsp-document-text.cpp
describe("updateLSPDocumentText", () => {
  it("set text", () => {
    let text = "";
    text = updateLSPDocumentText(text, { text: "content goes here" });
    assert.equal(text, "content goes here");
  });

  it("set text multiple times", () => {
    let text = "";
    text = updateLSPDocumentText(text, { text: "content goes here" });
    text = updateLSPDocumentText(text, { text: "newer content goes here" });
    assert.equal(text, "newer content goes here");
    text = updateLSPDocumentText(text, { text: "finally" });
    assert.equal(text, "finally");
  });

  it("set text range single line in middle of document same length", () => {
    let text = "";
    text = updateLSPDocumentText(text, { text: "content goes here" });
    text = updateLSPDocumentText(text, {
      text: "were",
      range: {
        start: { line: 0, character: 8 },
        end: { line: 0, character: 12 },
      },
    });
    assert.equal(text, "content were here");
  });

  it("set text range single line in middle of document smaller length", () => {
    let text = "";
    text = updateLSPDocumentText(text, { text: "content goes here" });
    text = updateLSPDocumentText(text, {
      range: {
        start: { line: 0, character: 8 },
        end: { line: 0, character: 12 },
      },
      text: "was",
    });
    assert.equal(text, "content was here");
  });

  it("set text range single line in middle of document larger length", () => {
    let text = "";
    text = updateLSPDocumentText(text, { text: "content goes here" });
    text = updateLSPDocumentText(text, {
      range: {
        start: { line: 0, character: 8 },
        end: { line: 0, character: 12 },
      },
      text: "might go somewhere",
    });
    assert.equal(text, "content might go somewhere here");
  });

  it("set text range delete line excluding line terminator", () => {
    let text = "";
    text = updateLSPDocumentText(text, { text: "hello\nworld\n" });
    text = updateLSPDocumentText(text, {
      range: {
        start: { line: 0, character: 0 },
        end: { line: 0, character: 1000 },
      },
      text: "",
    });
    assert.equal(text, "\nworld\n");
  });

  it("set text range delete line including line terminator", () => {
    let text = "";
    text = updateLSPDocumentText(text, { text: "hello\nworld\n" });
    text = updateLSPDocumentText(text, {
      range: {
        start: { line: 0, character: 0 },
        end: { line: 1, character: 0 },
      },
      text: "",
    });
    assert.equal(text, "world\n");
  });

  it("replace text multiple times", () => {
    let text = "";
    text = updateLSPDocumentText(text, { text: "content\ngoes\nhere" });
    text = updateLSPDocumentText(text, {
      range: {
        start: { line: 0, character: 7 },
        end: { line: 1, character: 3 },
      },
      text: "I wa",
    });
    text = updateLSPDocumentText(text, {
      range: {
        start: { line: 1, character: 0 },
        end: { line: 1, character: 0 },
      },
      text: "somew",
    });
    text = updateLSPDocumentText(text, {
      range: {
        start: { line: 0, character: 0 },
        end: { line: 0, character: 7 },
      },
      text: "",
    });
    assert.equal(text, "I was\nsomewhere");
  });
});

let lineTerminatorsExceptLSPS = ["\n", "\r", "\r\n"];

// See also: test/test-lsp-location.cpp
describe("indexFromLSPLocation", () => {
  it("offset from first line position", () => {
    let code = "hello\nworld";
    let o = indexFromLSPLocation(code, 0, 4);
    assert.equal(o, 4);
  });

  it("offset from second line position", () => {
    let code = "hello\nworld";
    let r = indexFromLSPLocation(code, 1, 2);
    assert.equal(r, 8);
  });

  it("offset from out of range line is end of file", () => {
    let code = "hello\nworld";
    let c = indexFromLSPLocation(code, 2, 2);
    assert.equal(c, code.length);
  });

  it("offset from beginning of line", () => {
    let code = "hello\nworld";
    let w = indexFromLSPLocation(code, 1, 0);
    assert.equal(w, 6);
  });

  it("offset from end of line", () => {
    let code = "hello\nworld";
    let terminator = indexFromLSPLocation(code, 0, 5);
    assert.equal(terminator, 5);
  });

  it("offset from empty line", () => {
    let code = "hello\n\nworld";
    for (let character of [0, 1, 2, 3, 4]) {
      let terminator = indexFromLSPLocation(code, 1, character);
      assert.equal(terminator, 6);
    }
  });

  it("offset from last character in line", () => {
    for (let lineTerminator of lineTerminatorsExceptLSPS) {
      let code = `hello${lineTerminator}world`;
      let o = indexFromLSPLocation(code, 0, 4);
      assert.equal(o, 4);
    }
  });

  it("offset from beyond end of line refers to line terminator", () => {
    for (let lineTerminator of lineTerminatorsExceptLSPS) {
      let code = `hello${lineTerminator}world`;
      let terminator = indexFromLSPLocation(code, 0, 6);
      assert.equal(terminator, 5);
    }
  });

  it("offset from beyond end of line containing non ascii refers to line terminator", () => {
    for (let lineTerminator of lineTerminatorsExceptLSPS) {
      let code = `hello \u{2603}!${lineTerminator}world`;
      let terminator = indexFromLSPLocation(code, 0, 9);
      assert.equal(terminator, "hello \u{2603}!".length);
    }
  });

  it("offset from end of last line", () => {
    let code = "hello";
    for (let character of [5, 6, 10]) {
      let terminator = indexFromLSPLocation(code, 0, character);
      assert.equal(terminator, 5);
    }
  });

  it("offset from end of last line containing non ascii", () => {
    let code = "hello \u2603!";
    for (let character of [8, 9, 15]) {
      let terminator = indexFromLSPLocation(code, 0, character);
      assert.equal(terminator, code.length);
    }
  });

  it("offset of inside cr lf gives beginning of cr lf", () => {
    let code = "hello\r\nworld";
    let terminator = indexFromLSPLocation(code, 0, "hello\r".length);
    assert.equal(terminator, "hello".length);
  });

  it("offset from empty input", () => {
    let code = "";
    for (let character of [0, 1, 10]) {
      let terminator = indexFromLSPLocation(code, 0, character);
      assert.equal(terminator, 0);
    }
  });

  it("offset from negative line", () => {
    let code = "hello\nworld";
    let c = indexFromLSPLocation(code, -2, 0);
    assert.equal(c, null);
  });

  it("offset from negative character", () => {
    let code = "hello\nworld";
    let c = indexFromLSPLocation(code, 1, -2);
    assert.equal(c, null);
  });

  it("offset after multi byte character", () => {
    // U+2603 has three UTF-8 code units: e2 98 83
    // U+2603 has one UTF-16 code unit: 2603
    let code = "\u{2603} x";
    let x = indexFromLSPLocation(code, 0, 2);
    assert.equal(x, "\u{2603} ".length);
  });

  it("offset after wide multi byte character", () => {
    // U+1f496 has four UTF-8 code units: f0 9f 92 96
    // U+1f496 has two UTF-16 code units: D83D DC96
    let code = "\u{0001f496} x";
    let x = indexFromLSPLocation(code, 0, 3);
    assert.equal(x, "\u{0001f496} ".length);
  });

  it("offset after multi byte character on middle line", () => {
    // U+2603 has three UTF-8 code units: e2 98 83
    // U+2603 has one UTF-16 code unit: 2603
    let code = "A\u{2603}a\nB\u{2603}b\nC\u{2603}c";
    let b = indexFromLSPLocation(code, 1, 2);
    assert.equal(b, "A\u{2603}a\nB\u{2603}".length);
  });
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
