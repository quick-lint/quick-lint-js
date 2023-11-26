#!/usr/bin/env node
// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import fs from "node:fs";
import path from "node:path";
import url from "node:url";
import { AnalyticsDB } from "./analytics-db.mjs";
import { loadConfigAsync } from "./config.mjs";

let __filename = url.fileURLToPath(import.meta.url);
let __dirname = path.dirname(__filename);

async function mainAsync() {
  let config = await loadConfigAsync();
  let db = AnalyticsDB.fromFile(config["db.file"]);

  let url = "https://quick-lint-js.com/";
  let now = Date.now();
  let oneMonthAgo = now - 30 * 24 * 60 * 60 * 1000;

  let { urls, counts } = db.countWebDownloadsComingFromURL(
    url,
    oneMonthAgo,
    now
  );

  let indices = [];
  let totalCount = 0;
  for (let i = 0; i < urls.length; ++i) {
    let urlIsPage = urls[i].endsWith("/");
    if (urlIsPage && urls[i] !== url) {
      indices.push(i);
      totalCount += counts[i];
    }
  }
  indices.sort((aIndex, bIndex) => {
    if (counts[aIndex] < counts[bIndex]) return +1;
    if (counts[aIndex] > counts[bIndex]) return -1;
    return 0;
  });
  let table = [];
  for (let index of indices) {
    table.push({
      URL: urls[index].replace("https://quick-lint-js.com", ""),
      Visits: counts[index],
      "Visits %": Number(((100 * counts[index]) / totalCount).toFixed(1)),
    });
  }
  console.table(table);
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
