// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import assert from "assert";
import child_process from "child_process";
import esbuild from "esbuild-wasm";
import fs from "fs";
import mime from "mime";
import path from "path";
import url from "url";

let __filename = url.fileURLToPath(import.meta.url);
let __dirname = path.dirname(__filename);

export class Router {
  constructor({ wwwRootPath }) {
    this._wwwRootPath = wwwRootPath;
  }

  get wwwRootPath() {
    return this._wwwRootPath;
  }

  async classifyDirectoryAsync(directoryPath) {
    if (isHiddenPath(directoryPath)) {
      return { type: "does-not-exist" };
    }

    let haveIndexEJSHTML = await isFileReadableAsync(
      path.join(this._wwwRootPath, directoryPath, "index.ejs.html")
    );
    let ancestorScripts = await this._loadAncestorScriptsAsync(directoryPath);
    let scriptForRoute = null;
    for (let script of ancestorScripts) {
      if (script.module.routes.hasOwnProperty(`/${directoryPath}`)) {
        scriptForRoute = script;
        break;
      }
    }
    let haveScriptForRoute = scriptForRoute !== null;
    let haveIndexHTML = await isFileReadableAsync(
      path.join(this._wwwRootPath, directoryPath, "index.html")
    );
    if (haveIndexEJSHTML + haveIndexHTML + haveScriptForRoute > 1) {
      return { type: "ambiguous" };
    } else if (haveIndexHTML) {
      return {
        type: "copy",
        path: path.join(directoryPath, "index.html"),
      };
    } else if (haveIndexEJSHTML) {
      return {
        type: "build-ejs",
        path: path.join(directoryPath, "index.ejs.html"),
      };
    } else if (haveScriptForRoute) {
      return {
        type: "routed",
        routerScript: scriptForRoute.script,
        routerDirectory: path.dirname(scriptForRoute.script),
      };
    } else {
      return { type: "does-not-exist" };
    }
  }

  async _loadAncestorScriptsAsync(directory) {
    let scripts = [];
    for (let d of ancestorHierarchy(directory)) {
      let scriptRelativePath = path.join(d, "index.mjs");
      let scriptPath = path.join(this._wwwRootPath, scriptRelativePath);
      try {
        let module = await import(url.pathToFileURL(scriptPath));
        scripts.push({
          script: scriptRelativePath,
          module: module,
        });
      } catch (e) {
        if (e.code === "ERR_MODULE_NOT_FOUND") {
          // Ignore.
        } else {
          throw e;
        }
      }
    }
    return scripts;
  }

  async classifyDirectoryRouteAsync(urlPath) {
    if (!urlPath.startsWith("/")) {
      throw new Error(`Invalid URL path: ${urlPath}`);
    }
    if (!urlPath.endsWith("/")) {
      throw new Error(`Invalid URL path: ${urlPath}`);
    }
    return await this.classifyDirectoryAsync(urlPath.replace(/^\//, ""));
  }

  async classifyFileAsync(filePath) {
    return await this.classifyFileRouteAsync("/" + filePath);
  }

  // Returned .type:
  //
  // .type === "missing": 404 Not Found
  // * .why === "broken-symlink": the file exists but cannot be read
  // * .why === "does-not-exist": the file does not exist
  // * .why === "ignored": the file might exist but shouldn't be used
  // * .why === "unreadable": the file exists but cannot be read
  // * .why === "unknown-extension": the file exists but content-type cannot be
  //                                 determined
  //
  // .type === "static": 200 OK
  // * .contentType: String content-type
  //
  // .type === "esbuild": 200 OK
  // * .esbuildConfig: Object to give to esbuild.build
  //
  // .type === "redirect": 200 OK
  // * .redirectTargetURL: String relative URL
  async classifyFileRouteAsync(urlPath) {
    if (path.basename(urlPath) === ".htaccess") {
      return { type: "forbidden", why: "server-config" };
    }
    if (path.basename(urlPath) === "index.mjs") {
      return { type: "index-script" };
    }

    if (isHiddenPath(urlPath)) {
      return { type: "missing", why: "ignored" };
    }

    let fullPath = path.join(this._wwwRootPath, urlPath);
    let readabilityError = await checkFileReadabilityAsync(fullPath);
    if (readabilityError !== null) {
      if (readabilityError === "does-not-exist") {
        let routerScriptPath = path.join(path.dirname(fullPath), "index.mjs");
        let haveParentIndexMJS = await isFileReadableAsync(routerScriptPath);
        if (haveParentIndexMJS) {
          let routerScriptRelativePath = path.relative(
            this._wwwRootPath,
            routerScriptPath
          );
          return {
            type: "routed",
            routerScript: routerScriptRelativePath,
            routerDirectory: path.dirname(routerScriptRelativePath),
          };
        }
      }
      return { type: "missing", why: readabilityError };
    }

    let contentType = mime.getType(urlPath);
    if (contentType === null) {
      return { type: "missing", why: "unknown-extension" };
    }
    let ignoredContentTypes = [
      // Don't serve README files.
      "text/markdown",
    ];
    if (ignoredContentTypes.includes(contentType)) {
      return { type: "missing", why: "ignored" };
    }
    if (/\.ejs\.html$/.test(urlPath)) {
      // Don't serve EJS-built HTML files directly.
      return { type: "missing", why: "ignored" };
    }
    if (path.basename(urlPath) === "index.html") {
      // Don't serve index.html directly. Caller must request the containing
      // directory instead.
      return { type: "missing", why: "ignored" };
    }
    return { type: "static", contentType: contentType };
  }

  async renderEJSFileAsync(ejsFilePath, { currentURI }) {
    let childProcess = await renderEJSChildProcessPool.takeAsync();
    try {
      let page = await childProcess.renderAsync({ currentURI, ejsFilePath });
      let pagePostProcess = page.replace(/<script>\s*\/\/\s*<\/script>/g, "");
      return pagePostProcess;
    } finally {
      renderEJSChildProcessPool.recycle(childProcess);
    }
  }

  async runESBuildAsync(esbuildConfig, outputPath) {
    await esbuild.build({
      ...esbuildConfig,
      bundle: true,
      entryPoints: esbuildConfig.entryPoints.map((uri) =>
        path.join(this._wwwRootPath, uri)
      ),
      outfile: outputPath,
    });
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

export function isHiddenPath(p) {
  return pathParts(p).some(
    (part) => part === "node_modules" || part.startsWith(".")
  );
}

function pathParts(p) {
  let parts = [];
  for (;;) {
    let dirname = path.dirname(p);
    if (dirname === p) {
      break;
    }
    parts.push(path.basename(p));
    p = dirname;
  }
  parts.reverse();
  return parts;
}

async function isFileReadableAsync(path) {
  try {
    await fs.promises.access(path, fs.constants.R_OK);
    return true;
  } catch (error) {
    if (error.syscall !== "access") {
      throw error;
    }
    return false;
  }
}

async function checkFileReadabilityAsync(path) {
  // The order of checks here is important.
  //
  // * Check lstat before stat to distinguish between symlink-doesn't-exist and
  //   symlink-target-doesn't-exist.
  // * Check stat before isFileReadableAsync, because isFileReadableAsync
  //   returns true on Windows when the symlink target doesn't exist.

  try {
    await fs.promises.lstat(path);
  } catch (error) {
    if (error.code === "ENOENT") {
      return "does-not-exist";
    }
    throw error;
  }

  try {
    await fs.promises.stat(path);
  } catch (error) {
    if (error.code === "ENOENT") {
      return "broken-symlink";
    }
    throw error;
  }

  if (await isFileReadableAsync(path)) {
    return null;
  }

  return "unreadable";
}

// "a/b/c" -> ["a/b/c", "a/b", "a", "."]
function ancestorHierarchy(p) {
  let result = [];
  for (;;) {
    result.push(p);
    if (p === ".") {
      break;
    }
    p = path.dirname(p);
  }
  return result;
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
