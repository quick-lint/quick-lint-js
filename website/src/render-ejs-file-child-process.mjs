// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

// This script is loaded as the script for a Node.js child process. This script
// communicates with the parent process using Node.js' IPC.
//
// This script's purpose is to isolate page loads from each other. The main
// advantage is allowing website developers to edit .mjs files then reload their
// the page in their browser without restarting the Node.js server.

import ejs from "ejs";
import fs from "fs";
import path from "path";
import url from "url";
import { getQuickLintJSVersionInfo } from "./qljs-version.mjs";

// The following imports aren't necessary, but they reduce latency for some
// EJS files by importing before we receive our first request.
import jsdom from "jsdom";

process.on("message", (message, _sendHandle) => {
  let { currentURI, ejsFilePath } = message;
  let result = renderEJSFileAsync({ currentURI, ejsFilePath })
    .then((result) => {
      process.send({ cmd: "result", result: result });
    })
    .catch((error) => {
      process.send({ cmd: "error", error: error });
    });
});
process.on("error", (error) => {
  if (error.code === "EPIPE" || error.code === "ERR_IPC_CHANNEL_CLOSED") {
    parentDied();
  } else {
    console.error("child:", error);
  }
});
process.on("disconnect", () => {
  parentDied();
});
process.send({ cmd: "ready" });

function parentDied() {
  // Our parent probably doesn't care about us anymore. Exit silently.
  process.exit(0);
}

async function renderEJSFileAsync({ currentURI, ejsFilePath }) {
  ejsFilePath = path.resolve(ejsFilePath);
  let ejsHTML = await fs.promises.readFile(ejsFilePath, "utf-8");

  function includer(_path, resolvedPath) {
    // include() (defined by our prelude) will restore the current working directory for us.
    process.chdir(path.dirname(resolvedPath));
  }

  let prelude = `
    let __realInclude = include;
    include = async function (...args) {
      let oldCWD = process.cwd();
      try {
        /* __realInclude will call includer which will call process.chdir. */
        return await __realInclude(...args);
      } finally {
        process.chdir(oldCWD);
      }
    }
  `;
  prelude = prelude.replace("\n", " "); // Preserve line numbers in user code.

  let oldCWD = process.cwd();
  process.chdir(path.dirname(ejsFilePath));
  try {
    return await ejs.render(
      `<% ${prelude} %>${ejsHTML}`,
      {
        currentURI: currentURI,
        importFileAsync: async (path) => {
          return await import(url.pathToFileURL(path));
        },
        makeRelativeURI: (uri) => {
          if (/^\w+:/.test(uri)) {
            return uri;
          }
          let suffix = uri.endsWith("/") ? "/" : "";
          return path.posix.relative(currentURI, uri) + suffix;
        },
        qljsVersionInfo: await getQuickLintJSVersionInfo(),
      },
      {
        async: true,
        compileDebug: true,
        filename: ejsFilePath,
        includer: includer,
      }
    );
  } finally {
    process.chdir(oldCWD);
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
