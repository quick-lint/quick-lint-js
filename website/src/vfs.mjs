// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import assert from "assert";
import esbuild from "esbuild-wasm";
import fs from "fs";
import mime from "mime";
import os from "os";
import path from "path";
import url from "url";
import { readFileAsync } from "./fs.mjs";
import { renderEJSFileAsync } from "./router.mjs";

export class VFS {
  constructor(rootPath) {
    this._rootPath = rootPath;
  }

  // Returns a DirectoryListing.
  async listDirectoryAsync(uri) {
    if (!uri.startsWith("/") || !uri.endsWith("/")) {
      throw new MalformedDirectoryURIError(uri);
    }
    let parts = uri.split("/");
    if (parts.includes(".") || parts.includes("..")) {
      throw new MalformedDirectoryURIError(uri);
    }

    let listing = new DirectoryListing();
    if (parts.some((p) => isIgnoredDirectoryComponent(p))) {
      // Ignore dotdirectories.
      return listing;
    }

    await this._loadFromRealFilesystemAsync(uri, listing);
    await this._loadFromIndexScriptsAsync(uri, listing);
    return listing;
  }

  async _loadFromRealFilesystemAsync(uri, listing) {
    let dirPath = path.join(this._rootPath, uri);
    let fsChildren;
    try {
      fsChildren = await fs.promises.readdir(dirPath, {
        encoding: "utf-8",
        withFileTypes: true,
      });
    } catch (e) {
      if (e.code === "ENOENT" || e.code === "ENOTDIR") {
        // Nothing to add.
        return;
      } else {
        throw e;
      }
    }

    for (let fsChild of fsChildren) {
      if (fsChild.isDirectory()) {
        if (isIgnoredDirectoryComponent(fsChild.name)) {
          // Ignore.
        } else {
          listing._addChild(fsChild.name, new VFSDirectory(uri, fsChild.name));
        }
      } else {
        if (fsChild.name === "index.html") {
          listing._addChild(
            "",
            new StaticVFSFile(path.join(dirPath, "index.html"))
          );
        } else if (fsChild.name === "index.ejs.html") {
          listing._addChild(
            "",
            new EJSVFSFile(path.join(dirPath, "index.ejs.html"), uri)
          );
        } else if (fsChild.name === "index.mjs") {
          // Ignore. index.mjs is imported later.
        } else if (fsChild.name === ".htaccess") {
          listing._addChild(
            fsChild.name,
            new ServerConfigVFSFile(path.join(dirPath, fsChild.name))
          );
        } else if (/\.ejs\.html$/.test(fsChild.name)) {
          // Don't serve EJS-built HTML files directly.
          // TODO(strager): Allow arbitrary .ejs.html files, accessible as .html
          // files.
        } else if (
          path.extname(fsChild.name) === "" ||
          fsChild.name.startsWith(".") ||
          /\.md$/.test(fsChild.name)
        ) {
          // Ignore.
        } else {
          listing._addChild(
            fsChild.name,
            new StaticVFSFile(path.join(dirPath, fsChild.name))
          );
        }
      }
    }
  }

  async _loadFromIndexScriptsAsync(uri, listing) {
    for (let currentURI of uriAncestry(uri)) {
      let indexScriptPath = path.join(this._rootPath, currentURI, "index.mjs");
      await this._loadFromIndexScriptAsync(uri, indexScriptPath, listing);
    }
  }

  async _loadFromIndexScriptAsync(uri, indexScriptPath, listing) {
    let routes;
    try {
      let indexModule = await import(url.pathToFileURL(indexScriptPath));
      routes = indexModule.routes;
    } catch (e) {
      if (e.code === "ERR_MODULE_NOT_FOUND" || e.code === "ENOTDIR") {
        return;
      } else {
        throw e;
      }
    }
    for (let routeURI in routes) {
      if (!Object.prototype.hasOwnProperty.call(routes, routeURI)) {
        continue;
      }
      if (!routeURI.startsWith(uri)) {
        continue;
      }
      let relativeRouteURI = routeURI.replace(uri, "");
      if (/\//.test(relativeRouteURI)) {
        let name = relativeRouteURI.split("/")[0];
        listing._addChild(name, new VFSDirectory(uri, name));
      } else {
        let route = routes[routeURI];
        let childEntry = route;
        if (
          typeof route === "object" &&
          Object.prototype.hasOwnProperty.call(route, "type")
        ) {
          switch (route.type) {
            case "build-ejs":
              // TODO(strager): The path should be relative to indexScriptPath's
              // parent directory instead.
              childEntry = new EJSVFSFile(
                path.join(this._rootPath, route.path),
                routeURI
              );
              break;

            case "esbuild":
              // TODO(strager): Path should be relative to indexScriptPath's
              // parent directory instead.
              childEntry = new ESBuildVFSFile({
                ...route.esbuildConfig,
                entryPoints: route.esbuildConfig.entryPoints.map((uri) =>
                  path.join(this._rootPath, uri)
                ),
              });
              break;

            default:
              // Unknown. Ignore.
              // TODO(strager): Should we be strict and error instead? We are
              // going to crash later anyway.
              break;
          }
        }
        listing._addChild(relativeRouteURI, childEntry);
      }
    }
  }
}

export class DirectoryListing {
  constructor() {
    this._entries = {};
  }

  // Returns names in sorted order.
  //
  // Includes an empty string representing the index file if one exists.
  names() {
    return Object.keys(this._entries).sort();
  }

  // Returns null if the name doesn't exist.
  //
  // If name is an empty string, the index file is returned (if one exists).
  get(name) {
    if (!Object.prototype.hasOwnProperty.call(this._entries, name)) {
      return null;
    }
    return this._entries[name];
  }

  _addChild(name, vfsEntry) {
    if (Object.prototype.hasOwnProperty.call(this._entries, name)) {
      let existingEntry = this._entries[name];
      if (name === "") {
        this._entries[""] = new IndexConflictVFSError([
          existingEntry,
          vfsEntry,
        ]);
      } else if (
        existingEntry instanceof VFSDirectory &&
        vfsEntry instanceof VFSDirectory
      ) {
        // Conflicting directories are okay.
      } else {
        throw new Error(`conflict for ${name}`);
      }
    } else {
      this._entries[name] = vfsEntry;
    }
  }
}

// Base class.
export class VFSEntry {}

// A regular file (or a symlink to a regular file).
export class StaticVFSFile extends VFSEntry {
  constructor(path) {
    super();
    this._path = path;
  }

  get path() {
    return this._path;
  }

  async getContentsAsync() {
    return await fs.promises.readFile(this._path);
  }

  getContentType() {
    return mime.getType(this._path);
  }
}

// An template file (usually generating HTML).
export class EJSVFSFile extends VFSEntry {
  constructor(path, uri) {
    super();
    this._path = path;
    this._uri = uri;
  }

  get path() {
    return this._path;
  }

  async getContentsAsync() {
    let data = await renderEJSFileAsync(this._path, {
      currentURI: this._uri,
    });
    return Buffer.from(data);
  }

  getContentType() {
    return "text/html";
  }
}

// A .js file generated by ESBuild.
export class ESBuildVFSFile extends VFSEntry {
  constructor(esbuildConfig) {
    super();
    this._esbuildConfig = esbuildConfig;
  }

  get config() {
    return this._esbuildConfig;
  }

  async getContentsAsync() {
    let temporaryDirectory = await fs.promises.mkdtemp(os.tmpdir() + path.sep);
    try {
      let bundlePath = path.join(temporaryDirectory, "bundle.js");
      await esbuild.build({
        ...this._esbuildConfig,
        bundle: true,
        outfile: bundlePath,
      });
      return await readFileAsync(bundlePath);
    } finally {
      fs.promises.rmdir(temporaryDirectory, { recursive: true });
    }
  }

  getContentType() {
    return "application/javascript";
  }
}

// A directory URI has multiple sources.
export class IndexConflictVFSError extends VFSEntry {
  constructor(conflictingVFSEntries) {
    super();
    this._conflictingVFSEntries = conflictingVFSEntries;
  }

  get conflictingPaths() {
    return this._conflictingVFSEntries.map((entry) => entry.path);
  }
}

// A subdirectory.
export class VFSDirectory extends VFSEntry {
  constructor(baseURI, name) {
    super();
    this._baseURI = baseURI;
    this._name = name;
  }

  get uri() {
    return `${this._baseURI}${this._name}/`;
  }
}

// A server configuration file which should be copied but not served.
//
// Example: .htaccess (for Apache httpd)
export class ServerConfigVFSFile extends StaticVFSFile {}

export class MalformedDirectoryURIError extends Error {
  constructor(uri) {
    super(`malformed directory URI: ${uri}`);
  }
}

// Exported for testing only.
export function uriAncestry(uri) {
  let result = [];
  for (;;) {
    result.push(uri);
    if (uri === "/") {
      break;
    }
    let slashIndex = uri.lastIndexOf("/", uri.length - 2);
    assert.notStrictEqual(slashIndex, -1);
    uri = uri.substr(0, slashIndex + 1);
  }
  return result;
}

function isIgnoredDirectoryComponent(name) {
  return name.startsWith(".") || name === "node_modules";
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
