// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

"use strict";

let vscode = require("vscode");
let { LanguageClient } = require("vscode-languageclient");

let clientID = "quick-lint-js";

let client = null;

function activate(context) {
  client = new LanguageClient(
    clientID,
    "quick-lint-js",
    {
      command: getQuickLintJSExecutablePath(),
      args: ["--lsp-server"],
    },
    {
      documentSelector: [{ language: "javascript" }],
    }
  );
  client.start();
}
exports.activate = activate;

function deactivate() {
  if (client === null) {
    return undefined;
  } else {
    return client.stop();
  }
}
exports.deactivate = deactivate;

function getQuickLintJSExecutablePath() {
  let path = vscode.workspace.getConfiguration(
    clientID
  )["exe-path"];
  let pathIsEmpty = /^\s*$/.test(path);
  return pathIsEmpty ? "quick-lint-js" : path;
}
