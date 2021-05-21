// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import { createProcessFactoryAsync } from "quick-lint-js-wasm/quick-lint-js.js";
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
    let parser = await process.createParserForWebDemoAsync();

    function lintAndUpdate() {
      synchronizeContent();

      // TODO(strager): On crash, show the error to the user.
      let input = codeInputElement.innerText;

      parser.setText(input);
      let marks = parser.lint();
      markEditorText(codeInputElement, window, marks);
      assignBoxErrorMessage(codeInputElement, marks);
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

function showErrorMessageBox(event, errorMessages) {
  const markedElement = event.target;
  const { code, message, severity } = errorMessages.filter(
    (arr) => markedElement === arr.mark
  )[0];
  const div = createErrorBox(markedElement, message, code, severity);
  let body = document.querySelector("body");
  body.appendChild(div);
}

function createErrorBox(markedElement, errorMessage, code, severity) {
  // TODO: Change background color based of the severity
  let div = document.createElement("div");
  const { bottom, left, height } = markedElement.getBoundingClientRect();
  div.setAttribute("id", "error-message-box");
  div.innerText = `${code} - ${errorMessage}`;
  div.setAttribute(
    "style",
    `position: absolute; 
    top: ${Math.trunc(bottom)}px; 
    left: ${Math.trunc(left)}px`
  );
  div.classList.add("error-box");
  return div;
}

function removeErrorMessageBox() {
  const errorMessagesBoxs = document.querySelectorAll("#error-message-box");
  errorMessagesBoxs.forEach((box) => box.remove());
}

function assignBoxErrorMessage(codeInputElement, marks) {
  elements = codeInputElement.querySelectorAll("mark");

  const errorMessages = marks.map(({ code, message, severity }, index) => ({
    code,
    message,
    severity,
    mark: elements[index],
  }));

  elements.forEach((mark) => {
    mark.addEventListener("mouseenter", (event) =>
      showErrorMessageBox(event, errorMessages)
    );
    mark.addEventListener("mouseout", removeErrorMessageBox);
  });
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
