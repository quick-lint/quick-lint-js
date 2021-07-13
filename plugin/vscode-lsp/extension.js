// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

"use strict";

let vscode = require("vscode");
let { LanguageClient } = require("vscode-languageclient");

let clientID = "quick-lint-js-lsp";

let client = null;
let toDispose = [];

async function startOrRestartServerAsync() {
  await stopServerIfStartedAsync();
  client = new LanguageClient(
    clientID,
    "quick-lint-js-lsp",
    {
      command: getQuickLintJSExecutablePath(),
      args: ["--lsp-server"],
    },
    {
      documentSelector: [{ language: "javascript" }, { language: "json" }],
    }
  );
  client.start();
}

async function stopServerIfStartedAsync() {
  if (client !== null) {
    try {
      await client.stop();
    } catch (error) {
      if (client.needsStop()) {
        throw error;
      } else {
        // Stopping failed, but the server stopped anyway.
        logError(error);
      }
    }
  }
}

async function activateAsync(context) {
  toDispose.push(
    vscode.workspace.onDidChangeConfiguration((event) => {
      logAsyncErrors(async () => {
        if (event.affectsConfiguration(`${clientID}.executablePath`)) {
          await startOrRestartServerAsync();
        }
      });
    })
  );

  await startOrRestartServerAsync();
}
exports.activate = activateAsync;

async function deactivateAsync() {
  await stopServerIfStartedAsync();
  for (let disposable of toDispose) {
    await disposable.dispose();
  }
}
exports.deactivate = deactivateAsync;

function getQuickLintJSExecutablePath() {
  let path = vscode.workspace.getConfiguration(clientID)["executablePath"];
  let pathIsEmpty = /^\s*$/.test(path);
  return pathIsEmpty ? "quick-lint-js" : path;
}

function logAsyncErrors(promise) {
  if (typeof promise === "function") {
    promise = promise();
  }
  return promise.catch((error) => {
    debugger;
    logError(error);
    return Promise.reject(error);
  });
}

function logError(error) {
  if (error && error.stack) {
    console.error("quick-lint-js-lsp error:", error.stack);
  } else {
    console.error("quick-lint-js-lsp error:", error);
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
