// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

import assert from "assert";
import jsdom from "jsdom";
import { markEditorText, sanitizeMarks } from "../public/demo/editor.mjs";

let dom = new jsdom.JSDOM("");

function preElementWithHTML(html) {
  let element = dom.window.document.createElement("pre");
  element.innerHTML = html;
  return element;
}

describe("markEditorText", () => {
  it("mark first word on line", () => {
    let editor = preElementWithHTML("hello world");
    markEditorText(editor, dom.window, [{ begin: 0, end: 5 }]);
    assert.strictEqual(editor.innerHTML, "<mark>hello</mark> world");
  });

  it("mark last word on line", () => {
    let editor = preElementWithHTML("hello world");
    markEditorText(editor, dom.window, [{ begin: 6, end: 11 }]);
    assert.strictEqual(editor.innerHTML, "hello <mark>world</mark>");
  });

  it("mark across two text nodes", () => {
    // <pre>helloworld</pre>
    let editor = dom.window.document.createElement("pre");
    editor.appendChild(dom.window.document.createTextNode("hello"));
    editor.appendChild(dom.window.document.createTextNode("world"));

    markEditorText(editor, dom.window, [{ begin: 3, end: 8 }]);
    assert.strictEqual(editor.innerHTML, "hel<mark>lowor</mark>ld");
  });

  it("marking deletes existing non-overlapping marks", () => {
    let editor = preElementWithHTML("<mark>hello</mark> world");
    markEditorText(editor, dom.window, [{ begin: 6, end: 11 }]);
    assert.strictEqual(editor.innerHTML, "hello <mark>world</mark>");
  });

  it("marking with no marks deletes existing marks", () => {
    let editor = preElementWithHTML("<mark>hello</mark> <mark>world</mark>");
    markEditorText(editor, dom.window, []);
    assert.strictEqual(editor.innerHTML, "hello world");
  });

  it("multiple new marks", () => {
    let editor = preElementWithHTML("hello world");
    markEditorText(editor, dom.window, [
      { begin: 0, end: 5 },
      { begin: 6, end: 11 },
    ]);
    assert.strictEqual(
      editor.innerHTML,
      "<mark>hello</mark> <mark>world</mark>"
    );
  });

  it("marking removes empty <mark>", () => {
    let editor = preElementWithHTML("<mark></mark> world");
    markEditorText(editor, dom.window, [{ begin: 1, end: 6 }]);
    assert.strictEqual(editor.innerHTML, " <mark>world</mark>");
  });

  it("marking preserves <br> immediately after mark", () => {
    let editor = preElementWithHTML("hello<br>world");
    markEditorText(editor, dom.window, [{ begin: 0, end: 5 }]);
    assert.strictEqual(editor.innerHTML, "<mark>hello</mark><br>world");
  });

  it("marking preserves <br> before inserted mark", () => {
    let editor = preElementWithHTML("one<br>twothree");
    markEditorText(editor, dom.window, [{ begin: 7, end: 7 + "three".length }]);
    assert.strictEqual(editor.innerHTML, "one<br>two<mark>three</mark>");
  });

  it("marking preserves <br> after inserted mark", () => {
    let editor = preElementWithHTML("onetwo<br>three");
    markEditorText(editor, dom.window, [{ begin: 0, end: 3 }]);
    assert.strictEqual(editor.innerHTML, "<mark>one</mark>two<br>three");
  });

  it("mark exactly over existing <mark>", () => {
    let editor = preElementWithHTML("<mark>hello</mark> world");
    markEditorText(editor, dom.window, [{ begin: 0, end: 5 }]);
    assert.strictEqual(editor.innerHTML, "<mark>hello</mark> world");
  });

  it("mark starts at end of existing <mark>", () => {
    let editor = preElementWithHTML("<mark>hello</mark>world");
    markEditorText(editor, dom.window, [{ begin: 5, end: 10 }]);
    assert.strictEqual(editor.innerHTML, "hello<mark>world</mark>");
  });

  it("add empty mark", () => {
    let editor = preElementWithHTML("helloworld");
    markEditorText(editor, dom.window, [{ begin: 5, end: 5 }]);
    assert.strictEqual(editor.innerHTML, "hello<mark></mark>world");
  });

  it("add empty mark immediately after non-empty mark", () => {
    let editor = preElementWithHTML("helloworld");
    markEditorText(editor, dom.window, [
      { begin: 0, end: 5 },
      { begin: 5, end: 5 },
    ]);
    assert.strictEqual(
      editor.innerHTML,
      "<mark>hello</mark><mark></mark>world"
    );
  });

  it("identical marks are merged", () => {
    let editor = preElementWithHTML("helloworld");
    markEditorText(editor, dom.window, [
      { begin: 0, end: 5 },
      { begin: 0, end: 5 },
    ]);
    assert.strictEqual(editor.innerHTML, "<mark>hello</mark>world");
  });
});

describe("sanitizeMarks", () => {
  it("marks are sorted before processing", () => {
    let marks = [
      { begin: 6, end: 11 },
      { begin: 0, end: 5 },
    ];
    assert.deepStrictEqual(sanitizeMarks(marks), [
      { begin: 0, end: 5 },
      { begin: 6, end: 11 },
    ]);
  });

  it("empty marks are preserved", () => {
    let marks = [{ begin: 6, end: 6 }];
    assert.deepStrictEqual(sanitizeMarks(marks), [{ begin: 6, end: 6 }]);
  });
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
