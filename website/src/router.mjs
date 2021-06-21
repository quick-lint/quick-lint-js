// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import ejs from "ejs";
import esbuild from "esbuild-wasm";
import fs from "fs";
import mime from "mime";
import path from "path";

export class Router {
  constructor({ esbuildBundles, htmlRedirects, wwwRootPath }) {
    this._esbuildBundles = esbuildBundles;
    this._htmlRedirects = htmlRedirects;
    this._wwwRootPath = wwwRootPath;
  }

  get esbuildBundles() {
    return this._esbuildBundles;
  }

  get htmlRedirects() {
    return this._htmlRedirects;
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
    let haveIndexHTML = await isFileReadableAsync(
      path.join(this._wwwRootPath, directoryPath, "index.html")
    );
    if (haveIndexEJSHTML && haveIndexHTML) {
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
    } else {
      return { type: "does-not-exist" };
    }
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
    if (Object.prototype.hasOwnProperty.call(this._htmlRedirects, urlPath)) {
      return {
        type: "redirect",
        redirectTargetURL: this._htmlRedirects[urlPath],
      };
    }
    if (Object.prototype.hasOwnProperty.call(this._esbuildBundles, urlPath)) {
      return {
        type: "esbuild",
        esbuildConfig: this._esbuildBundles[urlPath],
      };
    }

    if (isHiddenPath(urlPath)) {
      return { type: "missing", why: "ignored" };
    }
    let readabilityError = await checkFileReadabilityAsync(
      path.join(this._wwwRootPath, urlPath)
    );
    if (readabilityError !== null) {
      return { type: "missing", why: readabilityError };
    }

    let contentType = mime.getType(urlPath);
    if (contentType === null) {
      return { type: "missing", why: "unknown-extension" };
    }
    let ignoredContentTypes = [
      // Don't serve index.html directly. Caller must request the containing
      // directory instead.
      "text/html",

      // Don't serve README files.
      "text/markdown",
    ];
    if (ignoredContentTypes.includes(contentType)) {
      return { type: "missing", why: "ignored" };
    }
    return { type: "static", contentType: contentType };
  }

  async renderEJSFile(ejsFilePath, { currentURI }) {
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
          // __realInclude will call includer which will call process.chdir.
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

export function makeHTMLRedirect(redirectFrom, redirectTo) {
  return `<!DOCTYPE html>
<html>
  <head>
    <!-- ${redirectFrom} is an old link. Redirect users to ${redirectTo} instead. -->
    <meta charset="utf-8" />
    <link rel="canonical" href="${redirectTo}" />
    <meta http-equiv="refresh" content="0; url=${redirectTo}" />
  </head>
  <body>
    <p>
      This page has moved.
      <a href="${redirectTo}">Click here to go to the new location.</a>
    </p>
  </body>
</html>
`;
}

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
  if (await isFileReadableAsync(path)) {
    return null;
  }

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

  return "unreadable";
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
