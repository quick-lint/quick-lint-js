// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import { createProcessFactoryAsync } from "../../wasm/quick-lint-js.js";
import { markEditorText } from "./editor.mjs";
import {} from "../error-box.mjs";

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
