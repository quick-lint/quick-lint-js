// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import assert from "assert";
import colors from "colors/safe.js";
import {
  IndexConflictVFSError,
  MalformedDirectoryURIError,
  ServerConfigVFSFile,
  VFS,
  VFSDirectory,
} from "./vfs.mjs";
import { performance } from "perf_hooks";

export function makeServer({ wwwRootPath }) {
  let vfs = new VFS(wwwRootPath);
  return serve;

  function serve(request, response) {
    serveAsync(request, response).catch((error) => {
      console.error(`error processing request: ${error.stack}`);
      if (error instanceof MalformedDirectoryURIError) {
        response.writeHead(404); // TODO(strager): Should this be 400 instead?
        response.end();
        return;
      }

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

    let requestPath = request.url.match(/^[^?]+/)[0];
    let pathLastSlashIndex = requestPath.lastIndexOf("/");
    assert.notStrictEqual(pathLastSlashIndex, -1);
    let childName = requestPath.slice(pathLastSlashIndex + 1);
    let directoryURI = requestPath.slice(0, pathLastSlashIndex + 1);

    let listing = await vfs.listDirectoryAsync(directoryURI);
    let entry = listing.get(childName);
    if (entry === null || entry instanceof VFSDirectory) {
      response.writeHeader(404);
      response.end();
    } else if (entry instanceof ServerConfigVFSFile) {
      response.writeHeader(403);
      response.end();
    } else if (entry instanceof IndexConflictVFSError) {
      response.writeHeader(409);
      response.end();
    } else {
      let headers = { "content-type": entry.getContentType() };
      let data = undefined;
      if (request.method === "GET") {
        data = await entry.getContentsAsync();
      }
      response.writeHeader(200, headers);
      response.end(data);
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
