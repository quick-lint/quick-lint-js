// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

// For additional copyright information, see the following files:
// * dist/copyright.txt

// TODO(strager): Make this configurable.
// For build instructions, see demo/README.md.
let WASM_DEMO_JS_MODULE_PATH = "./dist/quick-lint-js-wasm-demo.js";

export async function loadQuickLintJS() {
  let loadQuickLintJSModule;
  if (typeof window === "undefined") {
    loadQuickLintJSModule = (await import(WASM_DEMO_JS_MODULE_PATH)).default;
  } else {
    let moduleURL = new URL(
      WASM_DEMO_JS_MODULE_PATH,
      window.location
    ).toString();
    await loadScript(moduleURL);
    loadQuickLintJSModule = window.QUICK_LINT_JS_WASM_DEMO;
  }
  let quickLintJSModule = await loadQuickLintJSModule();
  return new QuickLintJS(quickLintJSModule);
}

function loadScript(url) {
  return new Promise((resolve, reject) => {
    let script = window.document.createElement("script");
    script.onload = () => {
      resolve();
    };
    script.onerror = (error) => {
      reject(error);
    };
    script.src = url;
    window.document.body.appendChild(script);
  });
}

class QuickLintJS {
  constructor(module) {
    this._module = module;
    this._parseAndLint = module.cwrap(
      "quick_lint_js_parse_and_lint_for_wasm_demo",
      "number",
      ["string"]
    );
  }

  parseAndLint(input) {
    let errorsPointer = this._parseAndLint(input);
    let heap = this._module.HEAPU8.buffer;

    let rawErrorsU32 = new Uint32Array(heap, errorsPointer);
    let rawErrorsPtr = new Uint32Array(heap, errorsPointer);
    // See quick_lint_js::wasm_demo_error_reporter::error.
    let ERROR = {
      message: 0,
      begin_offset: 1,
      end_offset: 2,

      _ptr_size: 3,
      _u32_size: 3,
    };
    let marks = [];
    for (let i = 0; ; ++i) {
      let messagePtr = rawErrorsPtr[i * ERROR._ptr_size + ERROR.message];
      if (messagePtr === 0) {
        break;
      }
      let beginOffset = rawErrorsU32[i * ERROR._u32_size + ERROR.begin_offset];
      let endOffset = rawErrorsU32[i * ERROR._u32_size + ERROR.end_offset];
      marks.push({ begin: beginOffset, end: endOffset });
    }
    return marks;
  }
}

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
