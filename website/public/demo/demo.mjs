// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import { createProcessFactoryAsync } from "../../wasm/quick-lint-js.js";
import { markEditorText } from "./editor.mjs";

let codeInputElement = document.getElementById("code-input");
let shadowCodeInputElement = document.getElementById("shadow-code-input");

codeInputElement.addEventListener("scroll", (event) => {
  synchronizeScrolling();
});
synchronizeContent();

if (typeof window.ResizeObserver !== "undefined") {
  new window.ResizeObserver(synchronizeSize).observe(codeInputElement);
}

createProcessFactoryAsync()
  .then(async (processFactory) => {
    let process = await processFactory.createProcessAsync();
    let doc = await process.createDocumentForWebDemoAsync();

    function lintAndUpdate() {
      synchronizeContent();

      // TODO(strager): On crash, show the error to the user.
      let input = codeInputElement.value;
      doc.setText(input);
      let marks = doc.lint();
      markEditorText(shadowCodeInputElement, window, marks);
    }
    codeInputElement.addEventListener("input", (event) => {
      lintAndUpdate();
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

function synchronizeScrolling() {
  shadowCodeInputElement.scrollLeft = codeInputElement.scrollLeft;
  shadowCodeInputElement.scrollTop = codeInputElement.scrollTop;
}

function synchronizeSize() {
  shadowCodeInputElement.style.width = codeInputElement.style.width;
  shadowCodeInputElement.style.height = codeInputElement.style.height;
}

function showErrorMessageBox(mark, posCursorX) {
  const div = createErrorBox(
    mark,
    posCursorX,
    mark.attributes["data-message"].value,
    mark.attributes["data-code"].value,
    mark.attributes["data-severity"].value
  );
  let body = document.querySelector("body");
  body.appendChild(div);
}

function createErrorBox(
  markedElement,
  posCursorX,
  errorMessage,
  code,
  severity
) {
  // TODO: Change background color based of the severity
  let div = document.createElement("div");
  const { bottom } = markedElement.getBoundingClientRect();
  div.setAttribute("id", "error-box");
  div.innerText = `${code} - ${errorMessage}`;
  div.style.position = "fixed";
  div.style.overflow = "auto";
  div.style.top = `${Math.trunc(bottom)}px`;
  div.style.left = `${posCursorX}px`;
  return div;
}

function removeErrorMessageBox() {
  document.querySelector("#error-box")?.remove();
}

function showErrorMessage(event) {
  removeErrorMessageBox();

  const shadowInput = document.querySelector("#shadow-code-input");
  const marks = shadowInput.querySelectorAll("mark");
  for (let mark of marks) {
    const markRect = mark.getBoundingClientRect();
    if (cursorOverMark(event.clientX, event.clientY, markRect)) {
      showErrorMessageBox(mark, event.clientX);
      break;
    }
  }
}

function cursorOverMark(cursorPosX, cursorPosY, markRect) {
  const topDownIn = markRect.bottom >= cursorPosY && cursorPosY >= markRect.top;
  const leftRightIn =
    cursorPosX >= markRect.left && cursorPosX <= markRect.left + markRect.width;
  return topDownIn && leftRightIn;
}

document.addEventListener("DOMContentLoaded", () => {
  const codeInput = document.querySelector("#code-input");
  codeInput.addEventListener("mousemove", showErrorMessage);
  codeInput.addEventListener("input", removeErrorMessageBox);
  codeInput.addEventListener("click", removeErrorMessageBox);
  codeInput.addEventListener("mouseout", removeErrorMessageBox);
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
