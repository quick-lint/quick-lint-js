// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

import assert from "assert";
import jsdom from "jsdom";
import { markEditorText, sanitizeMarks } from "./editor.mjs";
import { loadQuickLintJS } from "./quick-lint-js.mjs";

let dom = new jsdom.JSDOM("");

function preElementWithHTML(html) {
  let element = dom.window.document.createElement("pre");
  element.innerHTML = html;
  return element;
}

let tests = {};

tests = {
  ...tests,

  "mark first word on line": () => {
    let editor = preElementWithHTML("hello world");
    markEditorText(editor, dom.window, [{ begin: 0, end: 5 }]);
    assert.strictEqual(editor.innerHTML, "<mark>hello</mark> world");
  },

  "mark last word on line": () => {
    let editor = preElementWithHTML("hello world");
    markEditorText(editor, dom.window, [{ begin: 6, end: 11 }]);
    assert.strictEqual(editor.innerHTML, "hello <mark>world</mark>");
  },

  "mark across two text nodes": () => {
    // <pre>helloworld</pre>
    let editor = dom.window.document.createElement("pre");
    editor.appendChild(dom.window.document.createTextNode("hello"));
    editor.appendChild(dom.window.document.createTextNode("world"));

    markEditorText(editor, dom.window, [{ begin: 3, end: 8 }]);
    assert.strictEqual(editor.innerHTML, "hel<mark>lowor</mark>ld");
  },

  "marking deletes existing non-overlapping marks": () => {
    let editor = preElementWithHTML("<mark>hello</mark> world");
    markEditorText(editor, dom.window, [{ begin: 6, end: 11 }]);
    assert.strictEqual(editor.innerHTML, "hello <mark>world</mark>");
  },

  "marking with no marks deletes existing marks": () => {
    let editor = preElementWithHTML("<mark>hello</mark> <mark>world</mark>");
    markEditorText(editor, dom.window, []);
    assert.strictEqual(editor.innerHTML, "hello world");
  },

  "multiple new marks": () => {
    let editor = preElementWithHTML("hello world");
    markEditorText(editor, dom.window, [
      { begin: 0, end: 5 },
      { begin: 6, end: 11 },
    ]);
    assert.strictEqual(
      editor.innerHTML,
      "<mark>hello</mark> <mark>world</mark>"
    );
  },

  "marking removes empty <mark>": () => {
    let editor = preElementWithHTML("<mark></mark> world");
    markEditorText(editor, dom.window, [{ begin: 1, end: 6 }]);
    assert.strictEqual(editor.innerHTML, " <mark>world</mark>");
  },

  "marking preserves <br> immediately after mark": () => {
    let editor = preElementWithHTML("hello<br>world");
    markEditorText(editor, dom.window, [{ begin: 0, end: 5 }]);
    assert.strictEqual(editor.innerHTML, "<mark>hello</mark><br>world");
  },

  "marking preserves <br> before inserted mark": () => {
    let editor = preElementWithHTML("one<br>twothree");
    markEditorText(editor, dom.window, [{ begin: 7, end: 7 + "three".length }]);
    assert.strictEqual(editor.innerHTML, "one<br>two<mark>three</mark>");
  },

  "marking preserves <br> after inserted mark": () => {
    let editor = preElementWithHTML("onetwo<br>three");
    markEditorText(editor, dom.window, [{ begin: 0, end: 3 }]);
    assert.strictEqual(editor.innerHTML, "<mark>one</mark>two<br>three");
  },

  "mark exactly over existing <mark>": () => {
    let editor = preElementWithHTML("<mark>hello</mark> world");
    markEditorText(editor, dom.window, [{ begin: 0, end: 5 }]);
    assert.strictEqual(editor.innerHTML, "<mark>hello</mark> world");
  },

  "mark starts at end of existing <mark>": () => {
    let editor = preElementWithHTML("<mark>hello</mark>world");
    markEditorText(editor, dom.window, [{ begin: 5, end: 10 }]);
    assert.strictEqual(editor.innerHTML, "hello<mark>world</mark>");
  },

  "add empty mark": () => {
    let editor = preElementWithHTML("helloworld");
    markEditorText(editor, dom.window, [{ begin: 5, end: 5 }]);
    assert.strictEqual(editor.innerHTML, "hello<mark></mark>world");
  },

  "add empty mark immediately after non-empty mark": () => {
    let editor = preElementWithHTML("helloworld");
    markEditorText(editor, dom.window, [
      { begin: 0, end: 5 },
      { begin: 5, end: 5 },
    ]);
    assert.strictEqual(
      editor.innerHTML,
      "<mark>hello</mark><mark></mark>world"
    );
  },
};

tests = {
  ...tests,

  "identical marks are merged": () => {
    let editor = preElementWithHTML("helloworld");
    markEditorText(editor, dom.window, [
      { begin: 0, end: 5 },
      { begin: 0, end: 5 },
    ]);
    assert.strictEqual(editor.innerHTML, "<mark>hello</mark>world");
  },

  "marks are sorted before processing": () => {
    let marks = [
      { begin: 6, end: 11 },
      { begin: 0, end: 5 },
    ];
    assert.deepStrictEqual(sanitizeMarks(marks), [
      { begin: 0, end: 5 },
      { begin: 6, end: 11 },
    ]);
  },

  "empty marks are preserved": () => {
    let marks = [{ begin: 6, end: 6 }];
    assert.deepStrictEqual(sanitizeMarks(marks), [{ begin: 6, end: 6 }]);
  },
};

tests = {
  ...tests,

  "parse and lint returns errors": async () => {
    let input = "undeclared_variable;\nanother_undeclared_variable;\n";
    let qljs = await loadQuickLintJS();
    let marks = qljs.parseAndLint(input);
    assert.deepStrictEqual(marks, [
      { begin: 0, end: "undeclared_variable".length },
      { begin: 21, end: 21 + "another_undeclared_variable".length },
    ]);
  },
};

async function main() {
  for (let testName in tests) {
    if (Object.prototype.hasOwnProperty.call(tests, testName)) {
      let test = tests[testName];
      console.log(`Running ${testName} ...`);
      await test();
    }
  }
  console.log("All tests passed");
}
main().catch((error) => {
  console.error(error.stack);
  process.exit(1);
});

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
