// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import { createProcessFactoryAsync } from "../../wasm/quick-lint-js.js";
import {} from "../error-box.mjs";

let codeInputElement = document.getElementById("code-input");
let codeInputMarksElement = document.getElementById("code-input-marks");
let codeInputMarksScrollerElement = document.getElementById(
  "code-input-marks-scroller"
);
let shadowCodeInputElement = document.getElementById("shadow-code-input");

codeInputElement.addEventListener("scroll", (event) => {
  synchronizeScrollingAndSize();
});
synchronizeContent();
synchronizeScrollingAndSize();

if (typeof window.ResizeObserver !== "undefined") {
  new window.ResizeObserver(synchronizeScrollingAndSize).observe(
    codeInputElement
  );
}

class FloatingEditorMarker {
  constructor(editorElement, marksContainerElement) {
    this._markWrapperElements = [];
    this._editorElement = editorElement;
    this._marksContainerElement = marksContainerElement;
  }

  setMarks(marks, text) {
    let textNode = this._editorElement.childNodes[0];
    if (typeof textNode === "undefined") {
      return;
    }

    let editorRect = this._editorElement.getBoundingClientRect();
    let offsetX = -editorRect.left + this._editorElement.scrollLeft;
    let offsetY = -editorRect.top + this._editorElement.scrollTop;

    let range = window.document.createRange();
    let markElementIndex = 0;
    // Place marks backwards. Otherwise, line n's mark underlines are hidden by
    // line n+1's mark backgrounds.
    for (let i = marks.length; i-- > 0; ) {
      let mark = marks[i];
      range.setStart(textNode, mark.begin);
      range.setEnd(textNode, mark.end);
      let rects = range.getClientRects();
      for (let r of rects) {
        if (r.width === 0) {
          // If a mark starts at the beginning of a line, Safari gives an extra
          // 0-width rectangle at the end of the previous line. Ignore this
          // extra rectangle.
          continue;
        }
        let { markElement, markWrapperElement } =
          this._getOrCreateMarkAndWrapper(markElementIndex++);
        markWrapperElement.style.display = "";
        markWrapperElement.style.left = `${r.left + offsetX}px`;
        // HACK(strager): Tweak the top so the mark's text aligns exactly with
        // the real text.
        markWrapperElement.style.top = `${Math.ceil(r.top + offsetY) + 1}px`;
        markWrapperElement.style.width = `${r.width}px`;
        markWrapperElement.style.height = `${r.height}px`;
        markElement.setAttribute("data-message", mark.message);
        markElement.setAttribute("data-code", mark.code);
        markElement.setAttribute("data-severity", mark.severity);
        markElement.textContent = text.slice(mark.begin, mark.end);
      }
    }

    for (let i = markElementIndex; i < this._markWrapperElements.length; ++i) {
      this._markWrapperElements[i].style.display = "none";
    }
  }

  _getOrCreateMarkAndWrapper(markElementIndex) {
    if (markElementIndex >= this._markWrapperElements.length) {
      return this._createMarkAndWrapper();
    }
    let markWrapperElement = this._markWrapperElements[markElementIndex];
    return {
      markElement: markWrapperElement.firstChild,
      markWrapperElement,
    };
  }

  // Create <div class="floating-mark><mark></mark></div>.
  //
  // The inner <mark> is shown visually.
  //
  // The outer .floating-mark <div> is for positioning.
  //
  // If we position the <mark> without a <div> wrapper, the mark underlines get
  // out of alignment. I don't know why.
  _createMarkAndWrapper() {
    let markElement = document.createElement("mark");

    let markWrapperElement = document.createElement("div");
    markWrapperElement.appendChild(markElement);
    markWrapperElement.classList.add("floating-mark");
    this._marksContainerElement.appendChild(markWrapperElement);
    this._markWrapperElements.push(markWrapperElement);

    return { markElement, markWrapperElement };
  }
}

createProcessFactoryAsync()
  .then(async (processFactory) => {
    async function createDocumentAsync() {
      let process = await processFactory.createProcessAsync();
      let doc = await process.createDocumentForWebDemoAsync();
      return doc;
    }

    let pendingDocument = null;
    let doc = null;

    function processCrashed() {
      // Make the next call to lintAndUpdate call restartProcessThenLint.
      doc = null;
      pendingDocument = null;
    }

    function restartProcessThenLint() {
      doc = null;
      pendingDocument = createDocumentAsync().then((newDoc) => {
        doc = newDoc;
        pendingDocument = null;
        lintAndUpdate();
      });
    }

    let marker = new FloatingEditorMarker(
      shadowCodeInputElement,
      codeInputMarksElement
    );

    function lintAndUpdate() {
      if (doc === null) {
        restartProcessThenLint();
        // restartProcess will call us later.
        return;
      }

      synchronizeContent();

      let input = codeInputElement.value;
      let marks;
      try {
        doc.setText(input);
        marks = doc.lint();
      } catch (e) {
        // TODO(strager): Show the error to the user.
        marks = [];
        processCrashed();
      }
      marker.setMarks(marks, input);
    }
    codeInputElement.addEventListener("input", (event) => {
      lintAndUpdate();
      synchronizeScrollingAndSize();
    });
    lintAndUpdate();
  })
  .catch((error) => {
    // TODO(strager): Show this error to the user.
    console.error(error);
  });

function synchronizeContent() {
  let input = codeInputElement.value;
  shadowCodeInputElement.textContent = input + "\n\n\n";
}

function synchronizeScrollingAndSize() {
  codeInputMarksElement.scrollWidth = codeInputElement.scrollWidth;
  codeInputMarksElement.scrollHeight = codeInputElement.scrollHeight;

  codeInputMarksScrollerElement.scroll({
    top: codeInputElement.scrollTop,
    left: codeInputElement.scrollLeft,
    behavior: "instant",
  });

  codeInputMarksScrollerElement.style.width = `${codeInputElement.offsetWidth}px`;
  codeInputMarksScrollerElement.style.height = `${codeInputElement.offsetHeight}px`;

  codeInputMarksElement.style.width = `${codeInputElement.scrollWidth}px`;
  codeInputMarksElement.style.height = `${codeInputElement.scrollHeight}px`;
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
