// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

function showErrorMessageBox(mark, posCursorX, diagnostics) {
  const div = createErrorBox(mark, posCursorX, diagnostics);
  let body = document.querySelector("body");
  body.appendChild(div);
}

function createErrorBox(markedElement, posCursorX, diagnostics) {
  // TODO: Change background color based of the severity
  let div = document.createElement("div");
  const { bottom } = markedElement.getBoundingClientRect();
  div.setAttribute("id", "error-box");
  div.innerText = diagnostics.map((d) => formatDiagnostic(d)).join("\n");
  div.style.position = "fixed";
  div.style.overflow = "auto";
  div.style.top = `${Math.trunc(bottom)}px`;
  div.style.left = `${posCursorX}px`;
  return div;
}

function removeErrorMessageBox() {
  let errorBoxElement = document.querySelector("#error-box");
  if (errorBoxElement !== null) {
    errorBoxElement.remove();
  }
}

function showErrorMessage(event, markedElement) {
  removeErrorMessageBox();

  const marks = markedElement.querySelectorAll("mark[data-message]");
  let diagnostics = [];
  let hoveredMarkElement = null;
  for (let mark of marks) {
    if (hasPlayingAnimation(mark)) {
      // Showing an error box on an animating mark would probably be confusing,
      // so don't.
      continue;
    }
    const markRect = mark.getBoundingClientRect();
    if (cursorOverMark(event.clientX, event.clientY, markRect)) {
      diagnostics.push(getDiagnosticFromMark(mark));
      // TODO(strager): Use the inner-most mark instead of the last mark.
      hoveredMarkElement = mark;
    }
  }
  if (hoveredMarkElement !== null) {
    showErrorMessageBox(hoveredMarkElement, event.clientX, diagnostics);
  }
}

function hasPlayingAnimation(element) {
  if (typeof element.getAnimations !== "function") {
    return false;
  }
  return element
    .getAnimations()
    .some((animation) => animation.playState === "running");
}

function cursorOverMark(cursorPosX, cursorPosY, markRect) {
  const topDownIn = markRect.bottom >= cursorPosY && cursorPosY >= markRect.top;
  const leftRightIn =
    cursorPosX >= markRect.left && cursorPosX <= markRect.left + markRect.width;
  return topDownIn && leftRightIn;
}

function formatDiagnostic(diagnostic) {
  return diagnostic.code
    ? `${diagnostic.code} - ${diagnostic.message}`
    : diagnostic.message;
}

function getDiagnosticFromMark(markElement) {
  return {
    message: markElement.attributes["data-message"].value,
    code: markElement.attributes["data-code"]?.value,
    severity: markElement.attributes["data-severity"].value,
  };
}

// hoveredElement is an Element which the user might hover over.
//
// markedElement is an Element which contains <mark> Elements as descendants.
//
// hoveredElement can be the same as markedElement.
function blessErrorMarksWithTooltip(hoveredElement, markedElement) {
  hoveredElement.addEventListener("mousemove", (event) => {
    showErrorMessage(event, markedElement);
  });
  hoveredElement.addEventListener("input", removeErrorMessageBox);
  hoveredElement.addEventListener("click", removeErrorMessageBox);
  hoveredElement.addEventListener("mouseout", removeErrorMessageBox);
}

document.addEventListener("DOMContentLoaded", () => {
  const codeInput = document.querySelector("#code-input");
  const codeInputMarks = document.querySelector("#code-input-marks");
  if (codeInput !== null && codeInputMarks !== null) {
    blessErrorMarksWithTooltip(codeInput, codeInputMarks);
  }

  for (let codeBlock of document.querySelectorAll("pre > code.javascript")) {
    blessErrorMarksWithTooltip(codeBlock, codeBlock);
  }
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
