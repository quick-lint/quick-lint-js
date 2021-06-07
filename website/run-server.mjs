// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import express from "express";
import http from "http";
import morgan from "morgan";
import path from "path";
import url from "url";
import { listenAsync, urlFromServerAddress } from "./src/net.mjs";
import { makeServer } from "./src/server.mjs";
import { websiteConfig } from "./src/config.mjs";

let __filename = url.fileURLToPath(import.meta.url);
let __dirname = path.dirname(__filename);

let DEFAULT_PORT = 9001;

async function mainAsync() {
  let { host, port } = parseArguments(process.argv.slice(2));

  let app = express();
  app.use(morgan("dev"));
  app.use(makeServer(websiteConfig));

  let server = http.createServer(app);
  await listenAsync(server, { host: host, port: port });
  console.log(`Server running: ${urlFromServerAddress(server.address())}`);
}

function parseArguments(args) {
  let host = "localhost";
  let port = DEFAULT_PORT;
  switch (args.length) {
    case 0:
      break;

    case 1:
      port = parseInt(args[0]);
      if (isNaN(port)) {
        throw new Error(`Expected port number, but got: ${args[0]}`);
      }
      break;

    case 2:
      host = args[0];
      port = parseInt(args[1]);
      if (isNaN(port)) {
        throw new Error(`Expected port number, but got: ${args[1]}`);
      }
      break;

    default:
      throw new Error("Too many arguments; expected 0, 1, or 2");
  }
  return { host, port };
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
