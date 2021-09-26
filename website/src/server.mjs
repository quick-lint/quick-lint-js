// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import ejs from "ejs";
import express from "express";
import fs from "fs";
import mime from "mime";
import os from "os";
import path from "path";
import { Router, makeHTMLRedirect } from "./router.mjs";

export function makeServer({
  esbuildBundles = {},
  htmlRedirects = {},
  wwwRootPath,
}) {
  let router = new Router({
    wwwRootPath: wwwRootPath,
    esbuildBundles: esbuildBundles,
    htmlRedirects: htmlRedirects,
  });

  let app = express.Router();
  app.get(/^\/((?:[^/]+\/)*)$/, serveDirectoryAsync);
  app.get(/^\/(.*)$/, serveFileAsync);
  return app;

  async function serveDirectoryAsync(request, response) {
    let classifiedDirectory = await router.classifyDirectoryRouteAsync(
      request.path
    );
    switch (classifiedDirectory.type) {
      case "ambiguous":
        response.writeHeader(409);
        response.end();
        return;

      case "build-ejs":
        let out = null;
        try {
          out = await router.renderEJSFile(
            path.join(router.wwwRootPath, classifiedDirectory.path),
            { currentURI: request.path }
          );
        } catch (error) {
          response.writeHeader(500, { "content-type": "text/plain" });
          response.end(error.stack);
          return;
        }
        response.writeHeader(200, { "content-type": "text/html" });
        response.end(out);
        return;

      case "copy":
        let html = await fs.promises.readFile(
          path.join(router.wwwRootPath, classifiedDirectory.path)
        );
        response.writeHeader(200, { "content-type": "text/html" });
        response.end(html);
        return;

      case "does-not-exist":
        response.writeHeader(404);
        response.end();
        return;

      default:
        throw new Error(
          `Unexpected type from classifyDirectoryAsync: ${classifiedDirectory.type}`
        );
    }
  }

  async function serveFileAsync(request, response) {
    let classification = await router.classifyFileRouteAsync(request.path);
    switch (classification.type) {
      case "missing":
        response.writeHeader(404);
        response.end();
        return;

      case "forbidden":
        response.writeHeader(403);
        response.end();
        return;

      case "static": {
        let filePath = path.join(router.wwwRootPath, request.path);
        let content = await fs.promises.readFile(filePath);
        response.writeHeader(200, {
          "content-type": classification.contentType,
        });
        response.end(content);
        return;
      }

      case "redirect":
        let redirectTo = classification.redirectTargetURL;
        response.writeHeader(200, { "content-type": "text/html" });
        response.end(makeHTMLRedirect(request.path, redirectTo));
        return;

      case "esbuild": {
        let temporaryDirectory = await fs.promises.mkdtemp(
          os.tmpdir() + path.sep
        );
        try {
          let bundlePath = path.join(temporaryDirectory, "bundle.js");
          try {
            await router.runESBuildAsync(
              classification.esbuildConfig,
              bundlePath
            );
          } catch (error) {
            response.writeHeader(500, { "content-type": "text/plain" });
            response.end(error.stack);
            return;
          }
          let content = await fs.promises.readFile(bundlePath);
          response.writeHeader(200, {
            "content-type": "application/javascript",
          });
          response.end(content);
        } finally {
          fs.promises.rmdir(temporaryDirectory, { recursive: true });
        }
        break;
      }

      default:
        throw new Error(
          `Unexpected type from classifyFileRouteAsync: ${classification.type}`
        );
    }
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
