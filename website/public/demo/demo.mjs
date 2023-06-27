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
        if (r.width === 0 && rects.length > 1) {
          // If a mark starts at the beginning of a line, Safari gives an extra
          // 0-width rectangle at the end of the previous line. Ignore this
          // extra rectangle.
          //
          // Note that zero-width rectangles are expected for diagnostics with
          // an empty span. For example, in the following code:
          //
          //   var i j;
          //       ^^ diagnostic between these two characters.
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
  // Make the scroller's size match the size of the code input's scroll region.
  //
  // If the scroller is too big, then scrolling will stop prematurely due to
  // Element#scroll's clamping.
  //
  // If the scroller is too small, then the marks will be visually clipped.
  codeInputMarksScrollerElement.style.width = `${codeInputElement.clientWidth}px`;
  codeInputMarksScrollerElement.style.height = `${codeInputElement.clientHeight}px`;

  // Make the marks container's size match the code input's virtual size.
  //
  // If the marks container is too small, then scrolling will stop prematurely
  // due to Element#scroll's clamping.
  //
  // If the marks container is too big, I haven't noticed any issues. It's
  // probably a bad idea to make the marks container too big, though.
  codeInputMarksElement.style.width = `${codeInputElement.scrollWidth}px`;
  codeInputMarksElement.style.height = `${codeInputElement.scrollHeight}px`;

  // Scroll the marks container so it aligns with how the user scrolled the code
  // input.
  let inputScrollTop = codeInputElement.scrollTop;
  let inputScrollLeft = codeInputElement.scrollLeft;
  codeInputMarksScrollerElement.scroll({
    top: inputScrollTop,
    left: inputScrollLeft,
    behavior: "instant",
  });
  let scrollerScrollTop = codeInputMarksScrollerElement.scrollTop;
  let scrollerScrollLeft = codeInputMarksScrollerElement.scrollLeft;

  // Element#scroll keeps the scrollTop and scrollLeft in bounds by clamping,
  // but because we adjusted the sizes of our scroller and marks container above
  // to match the code input, no clamping should occur.
  if (
    !(
      scrollerScrollTop === inputScrollTop &&
      scrollerScrollLeft === inputScrollLeft
    )
  ) {
    console.warn(
      "scrolling out of sync; tried to scroll to <%d,%d> but instead scrolled to <%d,%d>",
      inputScrollLeft,
      inputScrollTop,
      scrollerScrollLeft,
      scrollerScrollTop
    );
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
