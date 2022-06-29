// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import colors from "colors/safe.js";
import ejs from "ejs";
import fs from "fs";
import mime from "mime";
import os from "os";
import path from "path";
import url from "url";
import { Router } from "./router.mjs";
import { performance } from "perf_hooks";
import { readFileAsync } from "./fs.mjs";

export function makeServer({ wwwRootPath }) {
  let router = new Router({
    wwwRootPath: wwwRootPath,
  });
  return serve;

  function serve(request, response) {
    serveAsync(request, response).catch((error) => {
      console.error(`error processing request: ${error.stack}`);
      if (response.headersSent) {
        response.end();
      } else {
        response.writeHead(500);
        response.end(error.stack);
      }
    });
  }

  async function serveAsync(request, response) {
    logRequestResponse(request, response);
    if (request.method !== "GET" && request.method !== "HEAD") {
      response.writeHead(405);
      response.end(`bad method ${request.method}`);
      return;
    }
    if (!request.url.startsWith("/")) {
      response.writeHead(400);
      response.end(`bad URL ${request.url}`);
      return;
    }
    // TODO(strager): Don't modify request. Use a parameter instead.
    request.path = request.url.match(/^[^?]+/)[0];

    if (/^\/(?:[^/]+\/)*$/.test(request.path)) {
      await serveDirectoryAsync(request, response);
    } else {
      await serveFileAsync(request, response);
    }
  }

  async function serveDirectoryAsync(request, response) {
    let classifiedDirectory = await router.classifyDirectoryRouteAsync(
      request.path
    );
    await serveRouteAsync(request, response, classifiedDirectory);
  }

  async function serveFileAsync(request, response) {
    let classification = await router.classifyFileRouteAsync(request.path);
    await serveRouteAsync(request, response, classification);
  }

  async function serveRouteAsync(request, response, route) {
    switch (route.type) {
      case "ambiguous":
        response.writeHeader(409);
        response.end();
        return;

      case "build-ejs": {
        let headers = { "content-type": "text/html" };
        if (request.method === "HEAD") {
          // For HEAD requests, don't run the EJS because it might be slow.
          response.writeHeader(200, headers);
          response.end();
          return;
        }

        let out = null;
        try {
          out = await router.renderEJSFileAsync(
            path.join(router.wwwRootPath, route.path),
            { currentURI: request.path }
          );
        } catch (error) {
          response.writeHeader(500, { "content-type": "text/plain" });
          response.end(error.stack);
          return;
        }
        response.writeHeader(200, headers);
        response.end(out);
        return;
      }

      case "routed": {
        let routerScriptPath = path.join(
          router.wwwRootPath,
          route.routerScript
        );
        let { routes } = await import(url.pathToFileURL(routerScriptPath));
        if (!Object.prototype.hasOwnProperty.call(routes, request.path)) {
          response.writeHeader(404);
          response.end(`${request.path} is not routed by ${routerScriptPath}`);
          break;
        }
        let headers = { "content-type": "text/html" };
        if (request.method === "HEAD") {
          // For HEAD requests, don't run the EJS because it might be slow.
          response.writeHeader(200, headers);
          response.end();
          return;
        }
        await serveRouteAsync(request, response, routes[request.path]);
        return;
      }

      case "copy":
        let html = await readFileAsync(
          path.join(router.wwwRootPath, route.path)
        );
        response.writeHeader(200, { "content-type": "text/html" });
        response.end(html);
        return;

      case "does-not-exist":
        response.writeHeader(404);
        response.end();
        return;

      case "index-script": // Don't expose index.mjs to users.
      case "missing":
        response.writeHeader(404);
        response.end();
        return;

      case "forbidden":
        response.writeHeader(403);
        response.end();
        return;

      case "static": {
        let headers = { "content-type": route.contentType };
        if (request.method === "HEAD") {
          // For HEAD requests, don't waste time reading the file.
          response.writeHeader(200, headers);
          response.end();
          return;
        }

        let filePath = path.join(router.wwwRootPath, request.path);
        let content = await readFileAsync(filePath);
        response.writeHeader(200, headers);
        response.end(content);
        return;
      }

      case "esbuild": {
        let temporaryDirectory = await fs.promises.mkdtemp(
          os.tmpdir() + path.sep
        );
        try {
          let bundlePath = path.join(temporaryDirectory, "bundle.js");
          try {
            await router.runESBuildAsync(route.esbuildConfig, bundlePath);
          } catch (error) {
            response.writeHeader(500, { "content-type": "text/plain" });
            response.end(error.stack);
            return;
          }
          let content = await readFileAsync(bundlePath);
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
        throw new Error(`Unexpected route type: ${route.type}`);
    }
  }
}

let statusToColor = {
  // Key is the range; 2 means 200-299.
  2: colors.green,
  3: colors.cyan,
  4: colors.yellow,
  5: colors.red,
};

function logRequestResponse(request, response) {
  let beginTime = performance.now();
  response.on("finish", () => {
    let endTime = performance.now();
    let durationMilliseconds = endTime - beginTime;
    let statusColor =
      statusToColor[Math.floor(response.statusCode / 100)] ?? colors.red;
    console.log(
      `${request.method} ${request.url} ${statusColor(
        response.statusCode
      )} ${durationMilliseconds.toFixed(2)} ms`
    );
  });
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
