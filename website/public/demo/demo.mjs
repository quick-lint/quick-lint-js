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
      let input = codeInputElement.value;
      parser.setText(input);
      let marks = parser.lint();
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
