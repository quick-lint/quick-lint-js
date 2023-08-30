#!/usr/bin/env node
// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import glob from "glob";
import isbot from "isbot";
import util from "node:util";
import { AnalyticsDB } from "./analytics-db.mjs";
import { loadConfigAsync } from "./config.mjs";
import { parseLogFileAsync } from "./parse-log-file.mjs";

let globAsync = util.promisify(glob);

async function mainAsync() {
  let config = await loadConfigAsync();
  let db = AnalyticsDB.fromFile(config["db.file"]);

  function saveLogEntry(logEntry) {
    if (!(logEntry.httpMethod === "GET" && logEntry.finalStatus === 200)) {
      return;
    }
    if (isbot(logEntry["Apache_User-Agent"])) {
      return;
    }
    db.addWebDownload({
      timestamp: logEntry.requestTimestamp,
      url: `https://${logEntry.serverName}${logEntry.uri}`,
      downloaderIP: logEntry.remoteHostName,
      downloaderUserAgent: logEntry["Apache_User-Agent"],
    });
  }

  for (let logFile of await globAsync(config["apache2.log_files"])) {
    console.log(`importing from ${logFile} ...`);
    await parseLogFileAsync(
      logFile,
      {
        type: "apache",
        format: '%v %h %l %u %t "%r" %>s %b "%{Referer}i" "%{User-Agent}i"',
      },
      saveLogEntry
    );
  }

  db.close();
}

mainAsync().catch((e) => {
  console.error(e?.stack || e);
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
