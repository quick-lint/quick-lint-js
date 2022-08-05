// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import chokidar from "chokidar";
import cluster from "cluster";
import path from "path";
import url from "url";

let __filename = url.fileURLToPath(import.meta.url);
let __dirname = path.dirname(__filename);

async function mainAsync() {
  if (cluster.isPrimary) {
    let activeWorker = null;

    function makeNewWorker() {
      if (activeWorker !== null) {
        activeWorker.disconnect();
      }

      console.log("note: creating worker");
      let worker = cluster.fork();
      worker.on("disconnect", () => {
        if (activeWorker === worker) {
          console.error("error: worker disconnected; existing");
          process.exit(1);
        } else {
          console.log("note: old worker disconnected");
        }
      });

      activeWorker = worker;
    }

    let watcher = chokidar.watch(
      ["**/*.js", "**/*.mjs", "**/*.cjs", "**/*.wasm"],
      {
        cwd: __dirname,
      }
    );
    let watcherReady = false;
    watcher.on("error", (error) => {
      console.error(`warning: ${error}`);
    });
    watcher.on("all", (event, path) => {
      if (watcherReady) {
        makeNewWorker();
      }
    });
    watcher.on("ready", (event, path) => {
      watcherReady = true;
      makeNewWorker();
    });
  } else {
    let workerModule = await import("./run-server-worker.mjs");
    await workerModule.mainAsync();
  }
}

mainAsync().catch((error) => {
  console.error(error.stack);
  process.exit(1);
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
