// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import assert from "assert";
import child_process from "child_process";
import path from "path";
import url from "url";

let __filename = url.fileURLToPath(import.meta.url);
let __dirname = path.dirname(__filename);

export async function renderEJSFileAsync(ejsFilePath, { currentURI }) {
  let childProcess = await renderEJSChildProcessPool.takeAsync();
  try {
    let page = await childProcess.renderAsync({ currentURI, ejsFilePath });
    let pagePostProcess = page.replace(/<script>\s*\/\/\s*<\/script>/g, "");
    return pagePostProcess;
  } finally {
    renderEJSChildProcessPool.recycle(childProcess);
  }
}

class RenderEJSChildProcess {
  constructor({ child }) {
    this._child = child;

    this._errorPromise = new Promise((resolve, reject) => {
      child.on("message", (message, _sendHandle) => {
        if (message.cmd === "error") {
          reject(message.error);
        }
      });
      child.on("close", (_code, _signal) => {
        reject(new Error("worker exited without returning a response"));
      });
      child.on("exit", (_code, _signal) => {
        reject(new Error("worker exited without returning a response"));
      });
      child.on("error", (error) => {
        reject(error);
      });
      child.on("disconnect", () => {
        reject(new Error("worker disconnected without returning a response"));
      });
    });

    this._readyPromise = new Promise((resolve, reject) => {
      child.on("message", (message, _sendHandle) => {
        if (message.cmd === "ready") {
          resolve();
        }
      });
    });

    this._resultPromise = new Promise((resolve, reject) => {
      child.on("message", (message, _sendHandle) => {
        if (message.cmd === "result") {
          resolve(message.result);
        }
      });
    });
  }

  async waitForReadyAsync() {
    return await Promise.race([this._readyPromise, this._errorPromise]);
  }

  async renderAsync({ currentURI, ejsFilePath }) {
    let sent = this._child.send({ currentURI, ejsFilePath });
    assert.ok(sent);
    return await Promise.race([this._resultPromise, this._errorPromise]);
  }

  static fork() {
    let child = child_process.fork(
      path.join(__dirname, "render-ejs-file-child-process.mjs"),
      [],
      {
        serialization: "advanced",
      }
    );
    return new RenderEJSChildProcess({ child: child });
  }
}

class RenderEJSChildProcessPool {
  constructor() {
    let processCount = 6;

    this._processes = [];
    for (let i = 0; i < processCount; ++i) {
      this._forkProcess();
    }
  }

  async takeAsync() {
    if (this._processes.length === 0) {
      this._forkProcess();
    }
    let process = this._processes.shift();
    await process.waitForReadyAsync();
    return process;
  }

  recycle(_process) {
    // We can't use the old process because its module cache has been poisoned.
    this._forkProcess();
  }

  _forkProcess() {
    this._processes.push(RenderEJSChildProcess.fork());
  }
}

let renderEJSChildProcessPool = new RenderEJSChildProcessPool();

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
